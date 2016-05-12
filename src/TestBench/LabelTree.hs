{-# LANGUAGE DeriveFunctor #-}
{- |
   Module      : TestBench.LabelTree
   Description : Labelled rose-tree structure
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module TestBench.LabelTree where

--------------------------------------------------------------------------------

-- | A simple labelled rose-tree data structure.
data LabelTree a = Leaf a
                 | Branch String [LabelTree a]
  deriving (Eq, Ord, Show, Read, Functor)

toCustomTree :: (String -> a -> b) -> (String -> [b] -> b) -> LabelTree (String,a) -> b
toCustomTree lf br = go
  where
    go tr = case tr of
              Leaf (str,a)   -> lf str a
              Branch str trs -> br str (map go trs)
