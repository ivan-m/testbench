{-# LANGUAGE DeriveFunctor #-}

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

-- | A simple labelled rose-tree data structure also containing the depth.
data LabelTree a = Leaf !Int a
                 | Branch !Int String [LabelTree a]
  deriving (Eq, Ord, Show, Read, Functor)

foldLTree :: (Int -> String -> [a] -> a) -> (Int -> b -> a) -> LabelTree b -> a
foldLTree br lf = go
  where
    go tr = case tr of
              Leaf d b         -> lf d b
              Branch d str trs -> br d str (map go trs)

mapMaybeTree :: (a -> Maybe b) -> LabelTree a -> Maybe (LabelTree b)
mapMaybeTree f = go
  where
    go tr = case tr of
              Leaf d a       -> Leaf d <$> f a
              Branch d l trs -> case mapMaybe go trs of
                                  []   -> Nothing
                                  trs' -> Just (Branch d l trs')

mapMaybeForest :: (a -> Maybe b) -> (Int -> String -> [b] -> b) -> [LabelTree a] -> [b]
mapMaybeForest f br = mapMaybe (fmap (foldLTree br (flip const)) . mapMaybeTree f)
