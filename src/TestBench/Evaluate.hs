{-# LANGUAGE BangPatterns, OverloadedStrings #-}

{- |
   Module      : TestBench.Evaluate
   Description : Tree-based representation for Criterion and Weigh
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

An extremely simple rose tree-based representation of criterion
benchmarks and weigh measurements.

 -}
module TestBench.Evaluate
  ( -- * Types
    BenchTree
  , BenchForest
    -- * Conversion
  , flattenBenchTree
  , flattenBenchForest
    -- * Running benchmarks
  , benchmarkForest
  ) where

import TestBench.LabelTree

import Criterion.Analysis              (OutlierVariance (ovFraction),
                                        SampleAnalysis (..))
import Criterion.Internal              (runAndAnalyseOne)
import Criterion.Measurement           (initializeTime, secs)
import Criterion.Monad                 (withConfig)
import Criterion.Types                 (Benchmark, Benchmarkable, Config (..),
                                        DataRecord (..), Report (..),
                                        Verbosity (..), bench, bgroup)
import Statistics.Resampling.Bootstrap (Estimate (..))

import Data.List              (transpose)
import Text.PrettyPrint.Boxes

--------------------------------------------------------------------------------

-- | A more explicit tree-like structure for benchmarks than using
--   Criterion's 'Benchmark' type.
type BenchTree = LabelTree (String, Benchmarkable)

type BenchForest = [BenchTree]

flattenBenchTree :: BenchTree -> Benchmark
flattenBenchTree = toCustomTree (uncurry bench) bgroup

-- | Remove the explicit tree-like structure into the implicit one
--   used by Criterion.
--
--   Useful for embedding the results into an existing benchmark
--   suite.
flattenBenchForest :: BenchForest -> [Benchmark]
flattenBenchForest = map flattenBenchTree

-- | Run the specified benchmarks, printing the results (once they're
--   all complete) to stdout in a tabular format for easier
--   comparisons.
benchmarkForest :: Config -> BenchForest -> IO ()
benchmarkForest cfg bf = do initializeTime
                            rs <- toRows cfg bf
                            printBox (rowsToBox rs)

--------------------------------------------------------------------------------

data Row = Row { rowLabel  :: !String
               , rowDepth  :: !Int
               , rowResult :: !(Maybe Results)
               }
  deriving (Eq, Show, Read)

toRows :: Config -> BenchForest -> IO [Row]
toRows cfg = f2r 0
  where
    f2r :: Int -> BenchForest -> IO [Row]
    f2r !d = fmap concat . mapM (t2r d)

    t2r :: Int -> BenchTree -> IO [Row]
    t2r !d bt = case bt of
                  Leaf (lbl,b)  -> (:[]) <$> makeRow cfg lbl d b
                  Branch lbl ts -> (Row lbl d Nothing :)
                                   <$> f2r (d+1) ts

makeRow :: Config -> String -> Int -> Benchmarkable -> IO Row
makeRow cfg lbl d b = Row lbl d <$> getResults cfg lbl b

data Results = Results { resMean   :: !Estimate
                       , resStdDev :: !Estimate
                       , resOutVar :: !OutlierVariance
                       }
  deriving (Eq, Show, Read)

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

--------------------------------------------------------------------------------

rowsToBox :: [Row] -> Box
rowsToBox = hsep columnGap center1
            . withHead (vcat left) (vcat right)
            . transpose
            . ((empty11:resHeaders):) -- Add header row
            . map rowToBoxes

rowToBoxes :: Row -> [Box]
rowToBoxes r = moveRight (indentPerLevel * rowDepth r) (text (rowLabel r))
               : maybe blankRes resToBoxes (rowResult r)
  where
    blankRes = map (const empty11) resHeaders

empty11 :: Box
empty11 = emptyBox 1 1 -- Can't use nullBox, as /some/ size is needed.

indentPerLevel :: Int
indentPerLevel = 2

columnGap :: Int
columnGap = 2

resHeaders :: [Box]
resHeaders = ["Mean", "MeanLB", "MeanUB", "Stddev", "StddevLB", "StddevUB", "OutlierVariance"]

resToBoxes :: Results -> [Box]
resToBoxes r = e2b (resMean r) (e2b (resStdDev r) [ov])
  where
    e2b e bs = toB estPoint : toB estLowerBound : toB estUpperBound : bs
      where
        toB f = text (secs (f e))

    ov = text (show (round (ovFraction (resOutVar r) * 100) :: Int)) <> "%"

--------------------------------------------------------------------------------

withHead :: (a -> b) -> (a -> b) -> [a] -> [b]
withHead _  _  []    = []
withHead fh fr (h:r) = fh h : map fr r
