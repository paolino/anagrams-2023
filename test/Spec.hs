{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Data.Functor ((<&>))
import Data.List (permutations, uncons)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck
    ( Gen
    , Testable (property)
    , conjoin
    , elements
    , forAll
    , listOf1
    , scale
    , shuffle
    , (===)
    )
import Trie (collectPaths, load, unload)

-- Generate a string with only 3 letters to get a better chance of
-- superimposition of the prefixes when generating a dictionary.
-- >>> import Test.QuickCheck
-- >>> generate genWord
-- "bbcbaabc"
genWord :: Gen String
genWord = logScale' 1.2 $ listOf1 $ elements ['a' .. 'c']

-- Generate a list of words with a logarithmic scale of length.
-- long words are not very interesting.
-- >>> import Test.QuickCheck
-- >>> generate genDictionary
-- ["ba","acbbacbbccbabc","ccaabccb","cbabbbca","cbbacab","aacbc","bccbbbabbba"]
genDictionary :: Gen [String]
genDictionary = logScale' 1.3 $ listOf1 genWord

main :: IO ()
main = hspec $ do
    describe "respect invariants" $ do
        it "load . unload == id" $ do
            forAll genDictionary
                $ \xs -> property $ unload (load @Char xs) === unique xs
    describe "collectPaths" $ do
        it "collect one word" $ do
            forAll genDictionary
                $ \xs -> property $ do
                    w <- elements xs
                    let rs = collectPaths consume w (load @Char xs)
                    pure $ (w, []) `elem` rs
        it "collect all words" $ do
            forAll genDictionary
                $ \xs -> property
                    $ all
                        do \w -> (w, []) `elem` collectPaths consume w (load @Char xs)
                        do xs
        it "collect all words shuffled" $ do
            forAll
                do
                    -- we need to limit words to 6 letters, otherwise permutations
                    -- will generate too many extractions
                    ws <- fmap (take 6) <$> genDictionary
                    ws' <- traverse shuffle ws
                    pure $ zip ws ws'
                do
                    \xs ->
                        conjoin $ xs <&> \(w, w') -> do
                            (w, []) `elem` collectPaths consume w' (load @Char $ fst <$> xs)

unique :: [[Char]] -> [[Char]]
unique = Set.toList . Set.fromList

-- this is overkill, but it's a good test, in reality we'd just need
-- a list of all elements together with the rest of the list
-- >>> consume "abc"
-- [('a',"bc"),('b',"ac"),('c',"ba"),('b',"ca"),('c',"ab"),('a',"cb")]
consume :: String -> [(Char, String)]
consume xs = mapMaybe uncons $ permutations xs

-- | Adjust the QuickCheck @size@ parameter,
-- by replacing it with its logarithm with respect to some given basis.
logScale' :: Double -> Gen a -> Gen a
logScale' b = scale (floor @Double . logBase b . fromIntegral . succ)

-- >>> import Test.QuickCheck
-- >>> generate $ shuffle "abc"
-- "abc"
