{-# LANGUAGE GADTs, RankNTypes #-}

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

import qualified Data.DList as DL

import Control.Applicative (liftA2)
import Control.DeepSeq     (NFData)
import Control.Monad       (when, zipWithM_)
import Data.Int            (Int64)
import Data.Maybe          (isJust, mapMaybe)
import Text.Printf         (printf)

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

runGetWeight :: GetWeight -> IO Weight
runGetWeight (GetWeight f a) = (\(b,gc,_,_) -> Weight b gc) <$> weighFunc f a

data Weight = Weight { bytesAlloc :: !Int64
                     , numGC      :: !Int64
                     }
            deriving (Eq, Ord, Show, Read)

getWeight :: (NFData b) => (a -> b) -> a -> GetWeight
getWeight = GetWeight

flattenBenchTree :: EvalTree -> Maybe Benchmark
flattenBenchTree = fmap (foldLTree (const bgroup) (flip const))
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
                       hasStats <- getGCStatsEnabled
                       let ep' = ep { hasWeigh = hasWeigh ep && hasStats }
                           ec = EC cfg ep'
                       printHeaders ep'
                       toRows ec ef
  where
    ep = checkForest ef

data EvalParams = EP { hasBench  :: !Bool
                     , hasWeigh  :: !Bool
                     , nameWidth :: !Int
                     }
                deriving (Eq, Ord, Show, Read)

instance Monoid EvalParams where
  mempty = EP { hasBench  = False
              , hasWeigh  = False
              , nameWidth = 0
              }

  mappend ec1 ec2 = EP { hasBench  = mappendBy hasBench
                       , hasWeigh  = mappendBy hasWeigh
                       , nameWidth = nameWidth ec1 `max` nameWidth ec2
                       }
    where
      mappendBy f = f ec1 || f ec2

checkForest :: EvalForest -> EvalParams
checkForest = mconcat . map (foldLTree mergeNode calcConfig)
  where
    mergeNode d lbl ls = mempty { nameWidth = width d lbl }
                         `mappend` mconcat ls

    calcConfig d e = EP { hasBench  = isJust (eBench e)
                        , hasWeigh  = isJust (eWeigh e)
                        , nameWidth = width d (eName e)
                        }

    width d nm = indentPerLevel * d + length nm

data EvalConfig = EC { benchConfig :: {-# UNPACK #-}!Config
                     , evalParam   :: {-# UNPACK #-}!EvalParams
                     }
                deriving (Eq, Show, Read)

--------------------------------------------------------------------------------

type Path = [String]

data Row = Row { rowLabel  :: !String
               , rowPath   :: !Path -- ^ Invariant: length == rowDepth
               , rowDepth  :: {-# UNPACK #-} !Int
               , isLeaf    :: !Bool
               , rowBench  :: !(Maybe BenchResults)
               , rowWeight :: !(Maybe Weight)
               }
  deriving (Eq, Show, Read)

type PathList = DL.DList String

toRows :: EvalConfig -> EvalForest -> IO ()
toRows cfg = f2r DL.empty
  where
    f2r :: PathList -> EvalForest -> IO ()
    f2r pl = mapM_ (t2r pl)

    t2r :: PathList -> EvalTree -> IO ()
    t2r pl bt = case bt of
                  Leaf   d e      -> makeRow cfg pth d e >>= printRow ep
                  Branch d lbl ts -> do printRow ep (Row lbl pth d False Nothing Nothing)
                                        f2r (pl `DL.snoc` lbl) ts
      where
        pth = DL.toList pl

    ep = evalParam cfg

makeRow :: EvalConfig -> Path -> Int -> Eval -> IO Row
makeRow cfg pth d e = Row lbl pth d True
                      <$> tryRun hasBench eBench (getBenchResults (benchConfig cfg) lbl)
                      <*> tryRun hasWeigh eWeigh (fmap Just . runGetWeight)
  where
    lbl = eName e
    ep = evalParam cfg

    tryRun :: (EvalParams -> Bool) -> (Eval -> Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
    tryRun p f r =
      if p ep
         then maybe (return Nothing) r (f e)
         else return Nothing

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
    -- Set this here just in case someone didn't use the top-level
    -- 'testBench' function.
    cfg' = cfg { verbosity = Quiet }

    i = 0 -- We're ignoring this value anyway, so it should be OK to
          -- just set it.

--------------------------------------------------------------------------------

printHeaders :: EvalParams -> IO ()
printHeaders ep = do putStr (replicate (nameWidth ep) ' ')
                     when (hasBench ep) (mapM_ toPrintf benchHeaders)
                     when (hasWeigh ep) (mapM_ toPrintf weighHeaders)
                     putStr "\n"
  where
    toPrintf (w,hdr) = printf "%s%*s" columnSpace w hdr

printRow :: EvalParams -> Row -> IO ()
printRow ep r = do printf "%-*s" (nameWidth ep) label
                   when (hasBench ep) (printBench (rowBench r))
                   when (hasWeigh ep) (printWeigh (rowWeight r))
                   putStr "\n"
  where
    label :: String
    label = printf "%s%s" (replicate (rowDepth r * indentPerLevel) ' ') (rowLabel r)

indentPerLevel :: Int
indentPerLevel = 2

columnGap :: Int
columnGap = 2

columnSpace :: String
columnSpace = replicate columnGap ' '

benchHeaders :: [(Int, String)]
benchHeaders = map addWidth
                   ["Mean", "MeanLB", "MeanUB", "Stddev", "StddevLB", "StddevUB", "OutlierVariance"]

weighHeaders :: [(Int, String)]
weighHeaders = map addWidth ["AllocBytes", "NumGC"]

-- Maximum width a numeric field can take.  Might as well make them
-- all the same width.  All other formatters have been manually
-- adjusted to produce nothing longer than this.
secsWidth :: Int
secsWidth = length (secs ((-pi) / 000))

addWidth :: String -> (Int, String)
addWidth nm = (max (length nm) secsWidth, nm)

printBench :: Maybe BenchResults -> IO ()
printBench mr = zipWithM_ (printf "%s%*s" columnSpace) wdths cols
  where
    cols = maybe (repeat "")
                 (\r -> timed (resMean r) ++ timed (resStdDev r) ++ [ov r])
                 mr

    timed bs = [secIt estPoint, secIt estLowerBound, secIt estUpperBound]
      where
        secIt f = secs (f bs)

    ov r = percent (ovFraction (resOutVar r))

    wdths = map fst benchHeaders

printWeigh :: Maybe Weight -> IO ()
printWeigh mr = zipWithM_ (printf "%s%*s" columnSpace) wdths cols
  where
    cols = maybe (repeat "")
                 (\r -> [bytes (bytesAlloc r), count (numGC r)])
                 mr

    wdths = map fst weighHeaders

percent :: Double -> String
percent p = printf "%.3f%%" (p * 100)

-- | Human-readable description of the number of bytes used.  Assumed
--   non-negative.
bytes :: Int64 -> String
bytes b  = printf "%.3f %sB" val p
  where
    prefixes = [" ", "K", "M", "G", "T", "P", "E"] :: [String]

    base = 1024 :: Num a => a

    mult
      | b == 0    = 0
      | otherwise = floor (logBase (base :: Double) (fromIntegral b))

    val = fromIntegral b / (base ^ mult)

    p = prefixes !! mult

count :: Int64 -> String
count = printf "%.3e" . (`asTypeOf` (0::Double)) . fromIntegral
