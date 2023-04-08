{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Trie (
    Dict,
    Trie,
    load,
    renderDict,
    unload,
    dictionary,
    printLine,
    singleton,
    main,
    anagrams,
)
where

import Data.Bifunctor (second)
import Data.List (group, mapAccumL, sort, tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import System.Console.ANSI (
    Color (Green),
    ColorIntensity (Dull),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor),
    setSGRCode,
 )

-- | A trie is a recursive map from elements to tries.
newtype Trie a = Trie {trieMap :: Map a (Trie a)} deriving (Eq, Show)

instance Ord a => Semigroup (Trie a) where
    Trie a <> Trie b = Trie $ Map.unionWith (<>) a b

instance Ord a => Monoid (Trie a) where
    mempty = Trie Map.empty

-- | A path of a trie.
singleton :: [a] -> Trie a
singleton [] = Trie Map.empty
singleton (x : xs) = Trie $ Map.singleton x $ singleton xs

-- | Normalize a list of lists into a trie.
load :: Ord a => [[a]] -> Trie a
load = foldMap singleton

-- | Denormalize a trie into a list of lists.
unload :: Trie a -> [[a]]
unload (Trie m) = do
    (c, t) <- Map.toList m
    y <- unload t
    pure $ c : y

--------------------------------------------------------------------------------
-- bidirectional pattern synonyms to identify the tree characters in
-- matching and construction
--------------------------------------------------------------------------------

pattern Start :: Char
pattern Start = '└'

pattern Vert :: Char
pattern Vert = '│'

pattern Space :: Char
pattern Space = '·'

--------------------------------------------------------------------------------
-- unidirectional pattern synonyms to verify that a character is a graph
-- character
-- uses a view pattern to match on the result of a function
--------------------------------------------------------------------------------

pattern IsGraph :: Char
pattern IsGraph <- (flip elem [Start, Vert, Space] -> True)

--------------------------------------------------------------------------------
-- Rendering the trie as a tree
--------------------------------------------------------------------------------

type Dict = Trie Char

{- | Render a trie as a list of strings, where each string is a line of the
tree.
-}
render :: Dict -> [String]
render (Trie m) = do
    -- list monad
    (c, t) <- Map.toList m
    let ys = render t
    case ys of
        [] -> [[c]]
        (y : ys') -> (c : y) : ((Space :) <$> ys')

-- | An infinite list of spaces.
emptyLine :: String
emptyLine = repeat Space

-- | Append an infinite list of spaces to a string.
emptyTail :: String -> String
emptyTail = (<> emptyLine)

-- | Change the Space's in front of a suffix to form a path.
paths :: [String] -> [String]
paths xs = init $ foldr fixPrefix [emptyLine] xs
  where
    fixPrefix :: String -> [String] -> [String]
    fixPrefix cs rs@(ns : _) =
        zipWith
            fixPrefixChar
            (zip cs $ emptyTail $ tail cs)
            (emptyTail ns)
            : rs
    fixPrefix _ _ = error "impossible"

    -- receive a pair of adjacent characters and the character below the first
    fixPrefixChar :: (Char, Char) -> Char -> Char
    -- continue the path from below
    fixPrefixChar (Space, IsGraph) Start = Vert
    -- continue the path from below
    fixPrefixChar (Space, IsGraph) Vert = Vert
    -- respect the space below
    fixPrefixChar (Space, IsGraph) Space = Space
    -- start a new path
    fixPrefixChar (Space, _) _ = Start
    -- copy over the suffix, could be optimized
    fixPrefixChar (x, _) _ = x

-- | Render a trie of chars with a green graph in place of the empty prefixes.
renderDict :: Trie Char -> [String]
renderDict = paths . render

--------------------------------------------------------------------------------
-- test
--------------------------------------------------------------------------------

dictionary :: [String]
dictionary =
    [ "ability"
    , "accident"
    , "activity"
    , "actor"
    , "ad"
    , "addition"
    , "administration"
    , "advertising"
    , "advice"
    , "affair"
    , "agency"
    , "agreement"
    , "airport"
    , "alcohol"
    , "ambition"
    , "analysis"
    , "analyst"
    , "anxiety"
    , "apartment"
    , "appearance"
    , "apple"
    , "application"
    , "appointment"
    , "area"
    , "argument"
    , "army"
    , "arrival"
    , "art"
    , "article"
    , "artisan"
    , "aspect"
    , "assignment"
    , "assistance"
    , "assistant"
    , "association"
    , "assumption"
    , "atmosphere"
    , "attention"
    , "attitude"
    , "audience"
    , "awareness"
    ]

printLine :: String -> IO ()
printLine = putStrLn . concatMap change
  where
    change x@IsGraph =
        setSGRCode [SetColor Foreground Dull Green]
            <> [x]
            <> setSGRCode [Reset]
    change x = [x]

--------------------------------------------------------------------------------
-- main
--------------------------------------------------------------------------------

main :: IO ()
main =
    mapM_ printLine $ renderDict $ load dictionary

loadFile :: IO Dict
loadFile = do
    load . filter (\x -> length x > 0) . lines <$> readFile "data/words.txt"

renderBig = do
    t <- loadFile
    mapM_ printLine $ renderDict t

pattern Leaf :: Trie k
pattern Leaf <- (Map.null . trieMap -> True)

consumeLetters :: String -> String -> Dict -> [(String, String)]
consumeLetters rest path Leaf = [(reverse path, rest)]
consumeLetters xs path (Trie t) = do
    let l = length xs
    y : ys <- take l . fmap (take l) $ tails $ cycle xs
    case Map.lookup y t of
        Nothing -> []
        Just t' -> consumeLetters ys (y : path) t'

type Solution = Map String Int

addWord :: String -> Solution -> Solution
addWord x = Map.insertWith (+) x 1

consumeAllLetters :: String -> Dict -> Solution -> Set Solution -> (Set Solution, [Solution])
consumeAllLetters xs t actual memory = second concat $ mapAccumL go memory $ consumeLetters xs [] t
  where
    go memory (path, rest) =
        let new = addWord path actual
         in if Set.member new memory
                then (memory, [])
                else
                    let newMemory = Set.insert new memory
                     in case rest of
                            [] -> (newMemory, [new])
                            _ ->
                                let
                                    (newMemory', newSolutions) = consumeAllLetters rest t new newMemory
                                 in
                                    (newMemory', newSolutions)

{-             (path, rest) <- consumeLetters xs [] t
            let new = addWord path actual
            if Set.member new memory
                then []
                else
                    let newMemory = Set.insert new memory
                    in case rest of
                            [] -> (newMemory, [new])
                            _ -> let
                                consumeAllLetters rest t new newMemory -}

anagrams :: String -> Dict -> [String]
anagrams xs t = unwords . solutionWords <$> snd (consumeAllLetters xs t Map.empty Set.empty)

solutionWords :: Solution -> [String]
solutionWords = concatMap (\(w, n) -> replicate n w) . Map.assocs