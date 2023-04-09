{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

import Control.Monad (forM_)
import Data.Set qualified as Set
import Dict (dictionary)
import Test.Hspec (describe, hspec, it)
import Test.QuickCheck
    ( Gen
    , Testable (property)
    , classify
    , elements
    , forAll
    , label
    , listOf1
    , scale
    , shuffle
    , tabulate
    , (===)
    )
import Trie (collectPaths, load, unload)

genWord :: Gen String
genWord = listOf1 $ elements ['a' .. 'd']

genDictionary :: Gen [String]
genDictionary = logScale $ listOf1 genWord

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
            forAll genDictionary
                $ \xs -> property $ forM_ xs $ \w -> do
                    w' <- shuffle w
                    pure
                        $ label (show w)
                        $ (w, []) `elem` collectPaths consume w' (load @Char xs)

unique :: [[Char]] -> [[Char]]
unique = Set.toList . Set.fromList

consume :: String -> [(Char, String)]
consume [] = []
consume (x : xs) = [(x, xs)]

-- | Adjust the QuickCheck @size@ parameter,
-- by replacing it with its logarithm with respect to some given basis.
logScale' :: Double -> Gen a -> Gen a
logScale' b = scale (floor @Double . logBase b . fromIntegral . succ)

-- | Adjust the QuickCheck @size@ parameter,
-- by replacing it with its logarithm.
logScale :: Gen a -> Gen a
logScale = logScale' $ exp 1