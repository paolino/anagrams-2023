{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Dict
    ( Dict
    , renderDict
    , unload
    , dictionary
    , printLine
    , main
    )
where

import System.Console.ANSI
    ( Color (Green)
    , ColorIntensity (Dull)
    , ConsoleLayer (Foreground)
    , SGR (Reset, SetColor)
    , setSGRCode
    )
import Trie (Trie, destroy, load, unload)

-- | A trie of chars, useful to fast search for words in a dictionary.
type Dict = Trie Char

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

-- | Render a trie as a list of strings, where each string is a line of the
-- tree.
render :: Dict -> [String]
render = destroy (pure . pure) 
    $ \c x xs -> (c : x) : ((Space :) <$> xs)

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
