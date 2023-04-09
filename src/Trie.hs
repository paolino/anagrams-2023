{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE PatternSynonyms #-}

module Trie
    ( Trie
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
data Trie a = Trie
    { trieMap :: Map a (Trie a)
    , isPath :: Bool
    }
    deriving (Eq, Show)

instance Ord a => Semigroup (Trie a) where
    Trie a ia <> Trie b ib = Trie (Map.unionWith (<>) a b) (ia || ib)

instance Ord a => Monoid (Trie a) where
    mempty = Trie Map.empty False

-- | A path of a trie.
singleton :: Ord a => [a] -> Trie a
singleton [] = Trie Map.empty True
singleton (x : xs) = Trie (Map.singleton x $ singleton xs) False

-- | Normalize a list of lists into a trie.
load :: Ord a => [[a]] -> Trie a
load = foldMap singleton

-- | Denormalize a trie into a list of lists.
unload :: Trie a -> [[a]]
unload = destroy (pure . pure) $ \x y ys -> (x :) <$> y : ys

-- | A pattern to match a path.
pattern Path :: Trie a
pattern Path <- Trie _ True

-- | Destroy a trie.
destroy
    :: (a -> [b])
    -- ^ A function to end a path.
    -> (a -> b -> [b] -> [b])
    -- ^ A function to prepend a path.
    -> Trie a
    -- ^ The trie to destruct.
    -> [b]
destroy end cons (Trie m _) = do
    (c, t) <- Map.toList m
    case destroy end cons t of
        [] -> end c
        y : ys -> case t of 
            Path -> end c <> cons c y ys
            _ -> cons c y ys

-- | A function to match paths on a trie. We are given a source of elements
-- and a function to extract elements from the source. We return all possible
-- paths made of extracted elements and their leftover seeds.
collectPaths
    :: Ord a
    => (b -> [(a, b)])
    -- ^ Extractions from the source.
    -> b
    -- ^ The source.
    -> Trie a
    -- ^ The trie to traverse.
    -> [([a], b)]
    -- ^ All possible paths and their leftover seeds.
collectPaths f = go []
  where
    go acc source trie@(Trie t _) =
        case trie  of 
            Path -> (reverse acc, source) : rest 
            _ -> rest
        where
            rest = do
                (x, source') <- f source
                case Map.lookup x t of
                    Nothing -> []
                    Just t' -> go (x : acc) source' t'
