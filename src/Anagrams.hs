{-# LANGUAGE ImportQualifiedPost #-}

module Anagrams
    ( anagrams
    , main
    )
where

import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (second)
import Data.Char (isAlpha)
import Data.Function (fix)
import Data.List (mapAccumL, tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Dict (Dict)
import System.Console.Haskeline
    ( defaultSettings
    , getInputLine
    , outputStrLn
    , runInputT
    )
import System.Directory (listDirectory)
import System.FilePath (stripExtension, (<.>), (</>))
import Trie (collectPaths, load)

-- A string with its length.
data LString = LString
    { lString :: String
    , lStringLength :: Int
    }

-- Create an LString from a string.
mkLString :: String -> LString
mkLString = LString <*> length

-- Partially consume a word, returning a list of possible consumed
-- parts with the remaining parts.
consumePartial :: LString -> Dict -> [(String, LString)]
consumePartial = collectPaths $ \seed ->
    let l = lStringLength seed
        uncons [] = Nothing
        uncons (x : xs) = Just (x, LString xs (l - 1))
     in mapMaybe uncons $ take l . fmap (take l) $ tails $ cycle $ lString seed

-- A solution is a map from words to their number of occurrences.
type Solution = Map String Int

-- Add a word to a solution.
addWord :: String -> Solution -> Solution
addWord x = Map.insertWith (+) x 1

-- Consume a string, returning a set of solutions. Dynamic programming
-- is used to avoid recomputing the same solutions. Cache is a set of
-- solutions that have already been computed.
consumeAll
    :: LString
    -- ^ The string to consume.
    -> Dict
    -- ^ The dictionary.
    -> Solution
    -- ^ The current solution.
    -> Set Solution
    -- ^ The cache.
    -> (Set Solution, [Solution])
    -- ^ The new cache and the new solutions.
consumeAll challenge t actual memory =
    second concat
        $ mapAccumL go memory
        $ consumePartial challenge t
  where
    go memoryOld (path, rest)
        | Set.member new memoryOld = (memoryOld, [])
        | lStringLength rest <= 0 = (newMemory, [new])
        | otherwise = consumeAll rest t new newMemory
      where
        new = addWord path actual
        newMemory = Set.insert new memoryOld

-- | Find all anagrams of a string against a dictionary.
anagrams :: String -> Dict -> [String]
anagrams xs t =
    fmap (unwords . solution2Words)
        $ snd
        $ consumeAll (mkLString xs) t Map.empty Set.empty

-- Convert a solution to a list of words.
solution2Words :: Solution -> [String]
solution2Words = concatMap (\(w, n) -> replicate n w) . Map.assocs

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

-- Load a dictionary from a file.
loadFile :: FilePath -> IO Dict
loadFile fp = do
    load . filter (\x -> length x > 3) . lines <$> readFile ("data" </> fp <.> "txt")

listDictionary :: IO [FilePath]
listDictionary =
    mapMaybe (stripExtension ".txt")
        <$> listDirectory "data"

renderListOfDictionary :: [(Int, FilePath)] -> String
renderListOfDictionary =
    unlines
        . fmap (\(i, x) -> show i ++ ". " ++ x)

-- | Load a dictionary and run an interactive anagram finder.
main :: IO ()
main = do
    runInputT defaultSettings
        $ fix
        $ \loopFile -> do
            dictNames <- liftIO listDictionary
            let dictNames' = zip [1 ..] dictNames
            outputStrLn "\n\nAvailable dictionaries:"
            outputStrLn $ renderListOfDictionary dictNames'
            mfile <- getInputLine "Enter a number to select a dictionary: "
            case mfile of
                Nothing -> return ()
                Just numberString -> do
                    case reads numberString of
                        [(number, "")] -> do
                            case lookup number dictNames' of
                                Nothing -> do
                                    outputStrLn "Invalid number !"
                                    loopFile
                                Just dictName -> do
                                    t <- liftIO $ loadFile dictName
                                    fix
                                        $ \loop -> do
                                            minput <- getInputLine "Enter a string to anagram: "
                                            case minput of
                                                Nothing -> return ()
                                                Just input -> do
                                                    forM_ (anagrams (filter isAlpha input) t) outputStrLn
                                                    outputStrLn "-- End of anagrams --\n"
                                                    loop
                                    loopFile
                        _ -> do
                            outputStrLn "Not a number !"
                            loopFile
