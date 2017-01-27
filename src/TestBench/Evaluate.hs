{-# LANGUAGE BangPatterns, GADTs, OverloadedStrings, RankNTypes #-}

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
    EvalTree
  , EvalForest
  , Eval(..)
    -- ** Weights
  , GetWeight
  , getWeight
    -- * Conversion
  , flattenBenchTree
  , flattenBenchForest
    -- * Running benchmarks
  , evalForest
  ) where

import TestBench.LabelTree

import Criterion.Analysis              (OutlierVariance(ovFraction),
                                        SampleAnalysis(..))
import Criterion.Internal              (runAndAnalyseOne)
import Criterion.Measurement           (initializeTime, secs)
import Criterion.Monad                 (withConfig)
import Criterion.Types                 (Benchmark, Benchmarkable, Config(..),
                                        DataRecord(..), Report(..),
                                        Verbosity(..), bench, bgroup)
import GHC.Stats                       (getGCStatsEnabled)
import Statistics.Resampling.Bootstrap (Estimate(..))
import Weigh                           (weighFunc)

import Control.Applicative    (liftA2)
import Control.DeepSeq        (NFData)
import Control.Monad          (when)
import Data.Int               (Int64)
import Data.List              (transpose)
import Data.Maybe             (isJust, mapMaybe)
import Text.PrettyPrint.Boxes
import Text.Printf            (printf)

--------------------------------------------------------------------------------

-- | A more explicit tree-like structure for benchmarks than using
--   Criterion's 'Benchmark' type.
type EvalTree = LabelTree Eval

type EvalForest = [EvalTree]

data Eval = Eval { eName  :: !String
                 , eBench :: !(Maybe Benchmarkable)
                 , eWeigh :: !(Maybe GetWeight)
                 }

-- | The results from measuring memory usage.
data GetWeight where
  GetWeight :: forall a b. (NFData b) => (a -> b) -> a -> GetWeight

runGetWeight :: GetWeight -> IO (Maybe Weight)
runGetWeight (GetWeight f a) = do
  -- This should really be something that's cached...
  hasStats <- getGCStatsEnabled
  if hasStats
     then Just . uncurry Weight <$> weighFunc f a
     else return Nothing

data Weight = Weight { bytesAlloc :: !Int64
                     , numGC      :: !Int64
                     }
            deriving (Eq, Ord, Show, Read)

getWeight :: (NFData b) => (a -> b) -> a -> GetWeight
getWeight = GetWeight

flattenBenchTree :: EvalTree -> Maybe Benchmark
flattenBenchTree = fmap (foldLTree bgroup)
                   . mapMaybeTree (liftA2 fmap (bench . eName) eBench)

-- | Remove the explicit tree-like structure into the implicit one
--   used by Criterion.
--
--   Useful for embedding the results into an existing benchmark
--   suite.
flattenBenchForest :: EvalForest -> [Benchmark]
flattenBenchForest = mapMaybe flattenBenchTree

-- | Run the specified benchmarks, printing the results (once they're
--   all complete) to stdout in a tabular format for easier
--   comparisons.
evalForest :: Config -> EvalForest -> IO ()
evalForest cfg ef = do when (hasBench ep) initializeTime
                       rs <- toRows ec ef
                       printBox (rowsToBox rs)
  where
    ep = checkForest ef
    ec = EC cfg ep

data EvalParams = EP { hasBench :: !Bool
                     , hasWeigh :: !Bool
                     }
                deriving (Eq, Ord, Show, Read)

instance Monoid EvalParams where
  mempty = EP { hasBench = False
              , hasWeigh = False
              }

  mappend ec1 ec2 = EP { hasBench = mappendBy hasBench
                       , hasWeigh = mappendBy hasWeigh
                       }
    where
      mappendBy f = f ec1 || f ec2

checkForest :: EvalForest -> EvalParams
checkForest = mconcat . map (foldLTree (const mconcat) . fmap calcConfig)
  where
    calcConfig e = EP { hasBench = isJust (eBench e)
                      , hasWeigh = isJust (eWeigh e)
                      }

data EvalConfig = EC { benchConfig :: {-# UNPACK #-}!Config
                     , _evalParam  :: {-# UNPACK #-}!EvalParams
                     }
                deriving (Eq, Show, Read)

--------------------------------------------------------------------------------

data Row = Row { rowLabel  :: !String
               , rowDepth  :: {-# UNPACK #-} !Int
               , rowResult :: !(Maybe BenchResults)
               , rowWeight :: !(Maybe Weight)
               }
  deriving (Eq, Show, Read)

toRows :: EvalConfig -> EvalForest -> IO [Row]
toRows cfg = f2r 0
  where
    f2r :: Int -> EvalForest -> IO [Row]
    f2r !d = fmap concat . mapM (t2r d)

    t2r :: Int -> EvalTree -> IO [Row]
    t2r !d bt = case bt of
                  Leaf   e      -> (:[]) <$> makeRow cfg d e
                  Branch lbl ts -> (Row lbl d Nothing Nothing :)
                                   <$> f2r (d+1) ts

makeRow :: EvalConfig -> Int -> Eval -> IO Row
makeRow cfg d e = Row lbl d
                  <$> maybe (return Nothing) (getBenchResults (benchConfig cfg) lbl) (eBench e)
                  <*> maybe (return Nothing) runGetWeight (eWeigh e)
  where
    lbl = eName e

data BenchResults = BenchResults { resMean   :: !Estimate
                                 , resStdDev :: !Estimate
                                 , resOutVar :: !OutlierVariance
                                 }
  deriving (Eq, Show, Read)

getBenchResults :: Config -> String -> Benchmarkable -> IO (Maybe BenchResults)
getBenchResults cfg lbl b = do dr <- withConfig cfg' (runAndAnalyseOne i lbl b)
                               return $ case dr of
                                          Measurement{} -> Nothing
                                          Analysed rpt  -> Just $
                                            let sa = reportAnalysis rpt
                                            in BenchResults { resMean   = anMean sa
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
               : resToBoxes r

empty11 :: Box
empty11 = emptyBox 1 1 -- Can't use nullBox, as /some/ size is needed.

indentPerLevel :: Int
indentPerLevel = 2

columnGap :: Int
columnGap = 2

resHeaders :: [Box]
resHeaders = benchHeaders ++ weighHeaders

benchHeaders :: [Box]
benchHeaders = ["Mean", "MeanLB", "MeanUB", "Stddev", "StddevLB", "StddevUB", "OutlierVariance"]

weighHeaders :: [Box]
weighHeaders = ["AllocBytes", "NumGC"]

resToBoxes :: Row -> [Box]
resToBoxes r = maybe (blankHeaders benchHeaders) benchToBoxes (rowResult r)
               ++ maybe (blankHeaders weighHeaders) weightToBoxes (rowWeight r)
  where
    blankHeaders = map (const empty11)

benchToBoxes :: BenchResults -> [Box]
benchToBoxes r = e2b (resMean r) (e2b (resStdDev r) [ov])
  where
    e2b e bs = toB estPoint : toB estLowerBound : toB estUpperBound : bs
      where
        toB f = text (secs (f e))

    ov = text (show (round (ovFraction (resOutVar r) * 100) :: Int)) <> "%"

weightToBoxes :: Weight -> [Box]
weightToBoxes r = [ text (bytes (bytesAlloc r))
                  , text (show (numGC r))
                  ]

-- | Human-readable description of the number of bytes used.  Assumed
--   non-negative.
bytes :: Int64 -> String
bytes b  = printf "%.3f %sB" val p
  where
    prefixes = ["", "K", "M", "G", "T", "P", "E"] :: [String]

    base = 1024 :: Num a => a

    mult = floor (logBase (base :: Double) (fromIntegral b))

    val = fromIntegral b / (base ^ mult)

    p = prefixes !! mult

--------------------------------------------------------------------------------

withHead :: (a -> b) -> (a -> b) -> [a] -> [b]
withHead _  _  []    = []
withHead fh fr (h:r) = fh h : map fr r
