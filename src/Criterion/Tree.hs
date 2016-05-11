{-# LANGUAGE OverloadedStrings #-}
{- |
   Module      : Criterion.Tree
   Description : Tree-based representation for Criterion
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

An extremely simple rose tree-based representation of criterion
benchmarks.

 -}
module Criterion.Tree where

import Criterion
import Criterion.Analysis              (OutlierVariance (ovFraction),
                                        SampleAnalysis (..))
import Criterion.Measurement           (secs)
import Statistics.Resampling.Bootstrap (Estimate (..))

import Data.List              (transpose)
import Text.PrettyPrint.Boxes

--------------------------------------------------------------------------------

indentPerLevel :: Int
indentPerLevel = 2

data Results = Results { resMean   :: !Estimate
                       , resStdDev :: !Estimate
                       }
  deriving (Eq, Show, Read)

resHeaders :: [Box]
resHeaders = ["Mean", "MeanLB", "MeanUB", "Stddev", "StddevLB", "StddevUB"]

resToBoxes :: Results -> [Box]
resToBoxes r = e2b (resMean r) (e2b (resStdDev r) [])
  where
    e2b e bs = toB estPoint : toB estLowerBound : toB estUpperBound : bs
      where
        toB f = text (secs (f e))

data Row = Row { rowLabel  :: !String
               , rowDepth  :: !Int
               , rowResult :: !(Maybe Results)
               }
  deriving (Eq, Show, Read)

rowToBoxes :: Row -> [Box]
rowToBoxes r = moveRight (indentPerLevel * rowDepth r) (text (rowLabel r))
               : maybe blankRes resToBoxes (rowResult r)
  where
    blankRes = map (const (emptyBox 1 1)) resHeaders -- Can't use nullBox, needs to have _some_ size.

rowsToBox :: [Row] -> Box
rowsToBox = hcat center1 . withHead (vcat left) (vcat right) . transpose . map rowToBoxes

withHead :: (a -> b) -> (a -> b) -> [a] -> [b]
withHead _  _  []    = []
withHead fh fr (h:r) = fh h : map fr r
