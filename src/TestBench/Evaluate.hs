{-# LANGUAGE BangPatterns, FlexibleContexts, GADTs, OverloadedStrings,
             RankNTypes #-}

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
  , getWeightIO
    -- * Conversion
  , flattenBenchTree
  , flattenBenchForest
    -- * Running benchmarks
  , evalForest
    -- ** Weighing individual functions
  , weighIndex
  ) where

import TestBench.Commands  (resetUnusedConfig, weighFileArg, weighIndexArg)
import TestBench.LabelTree

import Criterion.Analysis    (OutlierVariance(ovFraction), SampleAnalysis(..))
import Criterion.Internal    (runAndAnalyseOne)
import Criterion.Measurement (initializeTime, secs)
import Criterion.Monad       (withConfig)
import Criterion.Types       (Benchmark, Benchmarkable, Config(..),
                              DataRecord(..), Report(..), bench, bgroup)
import Statistics.Types      (ConfInt(..), Estimate(..))
import Weigh                 (weighAction, weighFunc)

import Data.Csv (DefaultOrdered(..), Field, Name, ToField, ToNamedRecord(..),
                 ToRecord(..), header, namedRecord, record, toField)

import           Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Streaming    as B
import qualified Data.DList                   as DL
import           Streaming                    (Of, Stream, hoist)
import           Streaming.Cassava            (encodeByNameDefault)
import qualified Streaming.Prelude            as S

import Control.Applicative              (liftA2)
import Control.DeepSeq                  (NFData)
import Control.Monad                    (join, when, zipWithM_)
import Control.Monad.IO.Class           (liftIO)
import Control.Monad.Trans.Class        (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import Data.Functor                     (($>))
import Data.Int                         (Int64)
import Data.List                        (intercalate)
import Data.Maybe                       (isJust, listToMaybe, mapMaybe)
import Data.String                      (IsString)
import System.Environment               (getExecutablePath)
import System.Exit                      (ExitCode(..))
import System.IO                        (hClose)
import System.IO.Temp                   (withSystemTempFile)
import System.Process                   (rawSystem)
import Text.Printf                      (printf)

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
--
--   @since 0.2.0.0
data GetWeight where
  GetWeight   :: forall a b. (NFData b) => (a -> b)    -> a -> GetWeight
  GetWeightIO :: forall a b. (NFData b) => (a -> IO b) -> a -> GetWeight

runGetWeight :: GetWeight -> IO Weight
runGetWeight gw = mkWeight <$> case gw of
                                 GetWeight   f a -> weighFunc   f a
                                 GetWeightIO f a -> weighAction f a
  where
    mkWeight (b,gc,_,_) = Weight b gc

data Weight = Weight { bytesAlloc :: !Int64
                     , numGC      :: !Int64
                     }
            deriving (Eq, Ord, Show, Read)

-- | How to weigh a function.
--
--   @since 0.2.0.0
getWeight :: (NFData b) => (a -> b) -> a -> GetWeight
getWeight = GetWeight

-- | An IO-based variant of 'getWeight'.
--
--   @since 0.2.0.0
getWeightIO :: (NFData b) => (a -> IO b) -> a -> GetWeight
getWeightIO = GetWeightIO

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
                       let ec = EC cfg ep
                       printHeaders ep
                       (`evalStateT` zeroIndex) . maybeCSV . S.mapM printReturn $ toRows ec ef
  where
    ep = checkForest ef

    printReturn r = liftIO (printRow ep r) $> r

    maybeCSV = maybe S.effects streamCSV (csvFile cfg)

    -- In reality, this type signature contains StateT, but that
    -- over-complicates understanding what it does, and to specify it
    -- generically requires bringing in MonadBaseControl and
    -- MonadThrow.

    -- streamCSV :: FilePath -> Stream (Of Row) IO () -> IO ()
    streamCSV fp = runResourceT
                  . B.writeFile fp
                  . hoist lift
                  . encodeByNameDefault
                  . S.filter isLeaf

data EvalParams = EP { hasBench  :: !Bool
                     , hasWeigh  :: !Bool
                     , nameWidth :: !Width
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

type PathList = DL.DList String

data Row = Row { rowLabel  :: !String
               , rowPath   :: !PathList -- ^ Invariant: length == rowDepth
               , rowDepth  :: {-# UNPACK #-} !Depth
               , isLeaf    :: !Bool
               , rowBench  :: !(Maybe BenchResults)
               , rowWeight :: !(Maybe Weight)
               }
  deriving (Eq, Show, Read)

pathLabel :: Row -> String
pathLabel row = intercalate "/" (DL.toList (DL.snoc (rowPath row) (rowLabel row)))

-- | Unlike terminal output, this instance creates columns for benchmarks, weighing, etc. even if they're not used.
instance ToRecord Row where
  toRecord row =
    record (toField fmtLabel : benchRecord ++ weightRecord)
    where
      fmtLabel = pathLabel row

      benchRecord = timed resMean ++ timed resStdDev ++ [bField (ovFraction . resOutVar)]
        where
          bField = mField . (. rowBench) . fmap
          timed f = map (bField . (. f)) [estPoint, estLowerBound, estUpperBound]

      weightRecord = [wField bytesAlloc, wField numGC]
        where
          wField = mField . (. rowWeight) . fmap

      mField :: (ToField a) => (Row -> Maybe a) -> Field
      mField f = maybe mempty toField (f row)

-- | Unlike terminal output, this instance creates columns for benchmarks, weighing, etc. even if they're not used.
instance ToNamedRecord Row where
  toNamedRecord row =
    namedRecord (fmtLabel : benchRecord ++ weightRecord)
    where
      fmtLabel = (labelName, toField (pathLabel row))

      benchRecord = zip benchNames
                        (timed resMean ++ timed resStdDev ++ [bField (ovFraction . resOutVar)])
        where
          bField = mField . (. rowBench) . fmap
          timed f = map (bField . (. f)) [estPoint, estLowerBound, estUpperBound]

      weightRecord = zip weighNames
                         [wField bytesAlloc, wField numGC]
        where
          wField = mField . (. rowWeight) . fmap

      mField :: (ToField a) => (Row -> Maybe a) -> Field
      mField f = maybe mempty toField (f row)

instance DefaultOrdered Row where
  headerOrder _ = header (labelName : benchNames ++ weighNames)

toRows :: EvalConfig -> EvalForest -> Stream (Of Row) (StateT Index IO) ()
toRows cfg = f2r DL.empty
  where
    f2r :: PathList -> EvalForest -> Stream (Of Row) (StateT Index IO) ()
    f2r pl = mapM_ (t2r pl)

    t2r :: PathList -> EvalTree -> Stream (Of Row) (StateT Index IO) ()
    t2r pl bt = case bt of
                  Leaf   d e      -> do i <- lift get
                                        lift (put (i+1))
                                        r <- liftIO (makeRow cfg pl i d e)
                                        S.yield r
                  Branch d lbl ts -> S.cons (Row lbl pl d False Nothing Nothing)
                                            (f2r (pl `DL.snoc` lbl) ts)

makeRow :: EvalConfig -> PathList -> Index -> Depth -> Eval -> IO Row
makeRow cfg pl idx d e = Row lbl pl d True
                         <$> tryRun hasBench eBench (getBenchResults (benchConfig cfg) lbl)
                         <*> tryRun hasWeigh eWeigh (const (tryGetWeight idx))
  where
    lbl = eName e
    ep = evalParam cfg

    tryRun :: (EvalParams -> Bool) -> (Eval -> Maybe a) -> (a -> IO (Maybe b)) -> IO (Maybe b)
    tryRun p f r =
      if p ep
         then maybe (return Nothing) r (f e)
         else return Nothing

data BenchResults = BenchResults { resMean   :: !(Estimate ConfInt Double)
                                 , resStdDev :: !(Estimate ConfInt Double)
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
    --
    -- Also, just in case a CSV file is being outputted, don't try and
    -- write to it.
    cfg' = resetUnusedConfig cfg { csvFile = Nothing }

    i = 0 -- We're ignoring this value anyway, so it should be OK to
          -- just set it.

--------------------------------------------------------------------------------

type Index = Int

zeroIndex :: Index
zeroIndex = 0

tryGetWeight :: Index -> IO (Maybe Weight)
tryGetWeight idx = withSystemTempFile "testBench.weigh" $ \fp h -> do
  -- We use a temporary file in case the program prints something else
  -- out to stdout.
  hClose h -- We're not writing to it, just need the file
  exe <- getExecutablePath
  ec <- rawSystem exe ["--" ++ weighIndexArg, show idx, "--" ++ weighFileArg, fp, "+RTS", "-T", "-RTS"]
  case ec of
    ExitFailure{} -> return Nothing
    ExitSuccess   -> do
      out <- readFile fp
      case reads out of
        [(!mw,_)] -> return mw
        _         -> return Nothing

weighIndex :: EvalForest -> Index -> IO (Maybe Weight)
weighIndex ef = fmap join . mapM (mapM runGetWeight . eWeigh) . index es
  where
    es = concatMap leaves ef

index :: [a] -> Index -> Maybe a
index as n = listToMaybe . drop (n-1) $ as

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

type Width = Int

indentPerLevel :: Width
indentPerLevel = 2

columnGap :: Width
columnGap = 2

columnSpace :: String
columnSpace = replicate columnGap ' '

labelName :: Name
labelName = "Label"

benchNames :: (IsString str) => [str]
benchNames = ["Mean", "MeanLB", "MeanUB", "Stddev", "StddevLB", "StddevUB", "OutlierVariance"]

benchHeaders :: [(Width, String)]
benchHeaders = map addWidth benchNames

weighNames :: (IsString str) => [str]
weighNames = ["AllocBytes", "NumGC"]

weighHeaders :: [(Width, String)]
weighHeaders = map addWidth weighNames

-- Maximum width a numeric field can take.  Might as well make them
-- all the same width.  All other formatters have been manually
-- adjusted to produce nothing longer than this.
secsWidth :: Width
secsWidth = length (secs ((-pi) / 000))

addWidth :: String -> (Width, String)
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

estLowerBound :: (Num a) => Estimate ConfInt a -> a
estLowerBound e = estPoint e - confIntLDX (estError e)

estUpperBound :: (Num a) => Estimate ConfInt a -> a
estUpperBound e = estPoint e + confIntUDX (estError e)

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
