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

import TestBench.LabelTree

import Criterion.Analysis              (OutlierVariance (ovFraction),
                                        SampleAnalysis (..), analyseSample)
import Criterion.Internal              (runAndAnalyseOne)
import Criterion.Main.Options          (defaultConfig)
import Criterion.Measurement           (secs)
import Criterion.Monad                 (withConfig)
import Criterion.Types                 (Benchmarkable, Config (..),
                                        DataRecord (..), Report (..),
                                        Verbosity (..))
import Statistics.Resampling.Bootstrap (Estimate (..))

import Data.List              (transpose)
import Text.PrettyPrint.Boxes

--------------------------------------------------------------------------------

indentPerLevel :: Int
indentPerLevel = 2
-- | A more explicit tree-like structure for benchmarks than using
--   Criterion's 'Benchmark' type.
type BenchTree = LabelTree (String, Benchmarkable)

type BenchForest = [BenchTree]

flattenBenchTree :: BenchTree -> Benchmark
flattenBenchTree = toCustomTree (uncurry bench) bgroup

flattenBenchForest :: BenchForest -> [Benchmark]
flattenBenchForest = map flattenBenchTree

getResults :: Config -> String -> Benchmarkable -> IO (Maybe Results)
getResults cfg lbl b = do dr <- withConfig cfg' (runAndAnalyseOne i lbl b)
                          return $ case dr of
                                     Measurement{} -> Nothing
                                     Analysed rpt  -> Just $
                                       let sa = reportAnalysis rpt
                                       in Results { resMean   = anMean sa
                                                  , resStdDev = anStdDev sa
                                                  , resOutVar = anOutlierVar sa
                                                  }

  where
    cfg' = cfg { verbosity = Quiet }

    i = 0 -- We're ignoring this value anyway, so it should be OK to
          -- just set it.

data Results = Results { resMean   :: !Estimate
                       , resStdDev :: !Estimate
                       , resOutVar :: !OutlierVariance
                       }
  deriving (Eq, Show, Read)

resHeaders :: [Box]
resHeaders = ["Mean", "MeanLB", "MeanUB", "Stddev", "StddevLB", "StddevUB", "OutlierVariance"]

resToBoxes :: Results -> [Box]
resToBoxes r = e2b (resMean r) (e2b (resStdDev r) [ov])
  where
    e2b e bs = toB estPoint : toB estLowerBound : toB estUpperBound : bs
      where
        toB f = text (secs (f e))

    ov = text (show (round (ovFraction (resOutVar r) * 100) :: Int)) <> "%"

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
