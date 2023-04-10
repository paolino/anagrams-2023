{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Anagrams
    ( anagrams
    , main
    )
where

import Control.Applicative (Alternative (..))
import Control.Monad.Cont
    ( ContT (ContT, runContT)
    , MonadCont (..)
    , MonadIO (..)
    , MonadTrans (lift)
    , forM_
    )
import Control.Monad.Trans.Maybe (MaybeT (MaybeT, runMaybeT))
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
    ( InputT
    , defaultSettings
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

-- Load a dictionary from a file with a lower limits for letters in the words.
loadFile :: FilePath -> Int -> IO Dict
loadFile fp l = do
    load
        . filter (\x -> length x > l)
        . lines
        <$> readFile ("data" </> fp <.> "txt")

-- List all available dictionaries files in the "data" directory.
listDictionary :: IO [FilePath]
listDictionary = mapMaybe (stripExtension ".txt") <$> listDirectory "data"

-- Render a list of dictionaries for the user to select one.
renderListOfDictionary :: [(Int, FilePath)] -> String
renderListOfDictionary = unlines . fmap (\(i, x) -> show i ++ ". " ++ x)

-- Print a list of dictionaries and ask the user to select one.
listDictionaries :: InputT IO [(Int, FilePath)]
listDictionaries = do
    dictNames <- liftIO listDictionary
    let dictNamesMap = zip [1 :: Int ..] dictNames
    outputStrLn "\nAvailable dictionaries:\n"
    outputStrLn $ renderListOfDictionary dictNamesMap
    pure dictNamesMap

-- Parse a string as anything using Read instance but print a complaint when not possible.
parseAny :: (Read a, MonadIO m) => String -> InputT m (Maybe a)
parseAny input = do
    case reads input of
        [(anA, "")] -> do
            pure $ Just anA
        _ -> do
            outputStrLn "Not a valid input !"
            pure Nothing

-- Console inteaction monad.
type Interaction = ContT () (InputT IO)

-- Ask the user to enter a value and parse it. If the action fails,
-- the user is asked to enter a new value. If the user enters CTRL-D,
-- the interaction is aborted.
acquireValue
    :: Interaction b
    -- ^ Abort the interaction.
    -> InputT IO a
    -- ^ Pre action creates the context.
    -> String
    -- ^ Prompt.
    -> (String -> a -> InputT IO (Maybe b))
    -- ^ What to do with the input sting and the context. Nothing means
    -- the user entered an unacceptable value.
    -> Interaction b
acquireValue q pre prompt parse = fix $ \loop -> do
    x <- lift pre
    minput <- lift $ getInputLine $ "\n" <> prompt
    case minput of
        Nothing -> q
        Just input -> do
            mr <- lift $ parse input x
            maybe loop pure mr

-- Run an action indefinitely. The action receives a kill action that
-- aborts the loop.
withCtrlD :: (Interaction b -> Interaction ()) -> Interaction ()
withCtrlD f = callCC $ \kill -> fix $ \loop -> f (kill ()) >> loop

-- Main program. The user is asked to select a dictionary, then a
-- minimum number of letters for words, then a string to find anagrams
-- for.
-- All queries are aborted when the user enters CTRL-D, which resets the program
-- to the previous query.
-- The program is aborted when the user enters CTRL-D at the first query.
program :: Interaction ()
program = flip runContT pure $ do
    killProgram <- ContT withCtrlD
    d <- lift $ acquireValue
        do killProgram
        do listDictionaries
        do "Enter a number to select a dictionary: "
        do
            \ns ds -> runMaybeT $ do
                n <- MaybeT $ parseAny ns
                case lookup n ds of
                    Nothing -> do
                        lift $ outputStrLn "Invalid number !"
                        empty
                    Just dictName -> do
                        lift $ outputStrLn $ "... Loading dictionary " ++ dictName <> "\n"
                        pure dictName
    killLimit <- ContT withCtrlD
    j <- lift $ acquireValue
        do killLimit
        do pure ()
        do "Enter the minimum number of letters for words: "
        do \s _ -> parseAny s
    trie <- liftIO $ loadFile d j
    lift . lift $ outputStrLn "... Dictionary loaded.\n"
    killAnagram <- ContT withCtrlD
    w <- lift $ acquireValue
        do killAnagram
        do pure ()
        do "Enter a string to anagram: "
        do \s _ -> pure $ Just $ filter isAlpha s
    forM_ (anagrams w trie) $ lift . lift . outputStrLn

main :: IO ()
main = runInputT defaultSettings $ runContT program pure
