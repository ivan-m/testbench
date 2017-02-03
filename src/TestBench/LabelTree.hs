{-# LANGUAGE BangPatterns, DeriveFunctor #-}

{- |
   Module      : TestBench.LabelTree
   Description : Labelled rose-tree structure
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module TestBench.LabelTree where

import Data.Maybe (mapMaybe)

--------------------------------------------------------------------------------

-- | A simple labelled rose-tree data structure.
data LabelTree a = Leaf a
                 | Branch String [LabelTree a]
  deriving (Eq, Ord, Show, Read, Functor)

foldLTree :: (String -> [a] -> a) -> LabelTree a -> a
foldLTree br = go
  where
    go tr = case tr of
              Leaf a         -> a
              Branch str trs -> br str (map go trs)

mapMaybeTree :: (Int -> a -> Maybe b) -> LabelTree a -> Maybe (LabelTree b)
mapMaybeTree f = go 0
  where
    go !d tr = case tr of
                 Leaf a       -> Leaf <$> f d a
                 Branch l trs -> case mapMaybe (go (d+1)) trs of
                                   []   -> Nothing
                                   trs' -> Just (Branch l trs')

mapMaybeForest :: (Int -> a -> Maybe b) -> (String -> [b] -> b) -> [LabelTree a] -> [b]
mapMaybeForest f br = mapMaybe (fmap (foldLTree br) . mapMaybeTree f)
