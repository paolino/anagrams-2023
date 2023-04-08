{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Trie
    ( Trie
    , pattern Leaf
    , load
    , unload
    , singleton
    , destroy
    , collectPaths
    )
where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

-- | A trie is a recursive map from elements to tries.
newtype Trie a = Trie {trieMap :: Map a (Trie a)} deriving (Eq, Show)

instance Ord a => Semigroup (Trie a) where
    Trie a <> Trie b = Trie $ Map.unionWith (<>) a b

instance Ord a => Monoid (Trie a) where
    mempty = Trie Map.empty

-- | A pattern synonym for the empty trie.
pattern Leaf :: Ord a => Trie a
pattern Leaf <- (Map.null . trieMap -> True)
    where
        Leaf = mempty

-- | A path of a trie.
singleton :: Ord a => [a] -> Trie a
singleton [] = Leaf
singleton (x : xs) = Trie $ Map.singleton x $ singleton xs

-- | Normalize a list of lists into a trie.
load :: Ord a => [[a]] -> Trie a
load = foldMap singleton

-- | Denormalize a trie into a list of lists.
unload :: Trie a -> [[a]]
unload = destroy (pure . pure) $ \x y ys -> (x :) <$> y : ys

-- | Destroy a trie.
destroy
    :: (a -> [b])
    -- ^ A function to end a path.
    -> (a -> b -> [b] -> [b])
    -- ^ A function to prepend a path.
    -> Trie a
    -- ^ The trie to destruct.
    -> [b]
destroy end cons (Trie m) = do
    (c, t) <- Map.toList m
    case destroy end cons t of
        [] -> end c
        y : ys -> cons c y ys

-- | A function to match paths on a trie. We are given a source of elements
-- and a function to extract elements from the source. We return all possible
-- paths made of extracted elements and their leftover seeds.
collectPaths
    :: Ord a
    => (b -> [(a, b)]) -- ^ Extractions from the source.
    -> b -- ^ The source.
    -> Trie a -- ^ The trie to traverse.
    -> [([a], b)] -- ^ All possible paths and their leftover seeds.
collectPaths f = go []
  where
    go acc source Leaf = [(reverse acc, source)]
    go acc source (Trie t) = do
        (y, source') <- f source
        case Map.lookup y t of
            Nothing -> []
            Just t' -> go (y : acc) source' t'