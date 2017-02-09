{-# LANGUAGE ConstraintKinds, FlexibleContexts, FlexibleInstances,
             FunctionalDependencies, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, RankNTypes, RecordWildCards,
             ScopedTypeVariables #-}

{- |
   Module      : TestBench
   Description : Create tests and benchmarks together
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

Make it easier to compare benchmarks and to test that benchmarks are
indeed valid.

At the top level you will probably run the 'testBench' function, and
create comparisons using 'compareFunc' or 'compareFuncConstraint'.

For example:

> main :: IO ()
> main = testBench $ do
>   -- Compare how long it takes to make a list of the specified length.
>   compareFunc "List length"
>               (\n -> length (replicate n ()) == n)
>               [testWith (@? "Not as long as specified"), benchNormalForm]
>               (mapM_ (\n -> comp ("len == " ++ show n) n) [1..5])
>
>   -- Polymorphic comparisons.
>   --
>   -- Currently it isn't possible to use a Proxy as the argument to
>   -- the function (this will probably require Injective Type Families
>   -- in GHC 8.0), so we're using 'undefined' to specify the type.
>   compareFuncConstraint (Proxy :: Proxy (CUnion Eq Num))
>                         "Number type equality"
>                         (join (==) . (0`asTypeOf`))
>                         [baseline "Integer" (undefined :: Integer), benchNormalForm]
>                         $ do comp "Int"     (undefined :: Int)
>                              comp "Double"  (undefined :: Double)

When run, the output will look something like:

> Cases: 7  Tried: 7  Errors: 0  Failures: 0
>                           Mean    MeanLB    MeanUB    Stddev  StddevLB  StddevUB  OutlierVariance
> List length
>   len == 1            323.8 ns  318.6 ns  335.9 ns  23.86 ns  5.834 ns  40.90 ns              83%
>   len == 2            352.8 ns  349.1 ns  358.1 ns  15.05 ns  11.76 ns  19.62 ns              61%
>   len == 3            372.4 ns  358.4 ns  393.8 ns  62.50 ns  39.83 ns  90.85 ns              96%
>   len == 4            396.3 ns  378.4 ns  419.2 ns  67.83 ns  46.71 ns  94.74 ns              96%
>   len == 5            426.0 ns  407.0 ns  459.5 ns  82.23 ns  53.37 ns  110.2 ns              97%
> Number type equality
>   Integer             75.43 ns  74.48 ns  76.71 ns  3.615 ns  2.748 ns  5.524 ns              69%
>   Int                 74.39 ns  73.48 ns  76.24 ns  3.964 ns  2.500 ns  7.235 ns              74%
>   Double              78.05 ns  75.84 ns  82.50 ns  9.790 ns  6.133 ns  16.99 ns              94%

 -}
module TestBench
  ( -- * Specification and running
    TestBench
  , testBench
  , testBenchWith
    -- ** Running manually
  , getTestBenches
  , EvalTree
  , EvalForest
  , flattenBenchForest
  , evalForest
    -- ** Lower-level types
  , TestBenchM
  , OpTree
  , Operation
  , LabelTree(..)

    -- * Grouping
  , collection

    -- * Direct benchmarks\/tests
  , nfEq
  , whnfEq
  , mkTestBench

    -- * Comparisons
  , compareFunc
  , compareFuncConstraint

    -- ** Specifying constraints
  , CUnion

    -- ** Comparison parameters
  , CompParams
    -- *** Control benchmarking
  , benchNormalForm
  , benchIO
  , benchNormalFormIO
  , withBenchMode
  , noBenchmarks

    -- *** Control testing
  , baseline
  , testWith
  , noTests

    -- *** Control function weighing
  , GetWeight
  , weigh

    -- ** Specify comparisons
  , Comparison
  , comp
  , compBench
  , compTest

    -- ** Lower-level types
  , ComparisonM
  , SameAs
  ) where

import TestBench.Commands
import TestBench.Constraints
import TestBench.Evaluate
import TestBench.LabelTree

import Criterion       (Benchmarkable, nf, nfIO, whnf, whnfIO)
import Criterion.Types (Config)
import Test.HUnit.Base (Assertion, Counts(..), Test(..), (@=?), (~:))
import Test.HUnit.Text (runTestTT)

import Control.Arrow                   ((&&&))
import Control.DeepSeq                 (NFData(..))
import Control.Monad                   (when)
import Control.Monad.IO.Class          (MonadIO(liftIO))
import Control.Monad.Trans.Class       (lift)
import Control.Monad.Trans.Reader      (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Writer.Lazy (WriterT, execWriterT, tell)
import Data.Maybe                      (mapMaybe)
import Data.Monoid                     (Endo(..))
import Data.Proxy                      (Proxy(..))
import Options.Applicative             (execParser)
import System.Exit                     (exitSuccess)

-- -----------------------------------------------------------------------------

-- | An individual operation potentially consisting of a benchmark
--   and/or test.
data Operation = Op { opName  :: !String
                    , opBench :: !(Maybe Benchmarkable)
                    , opWeigh :: !(Maybe GetWeight)
                    , opTest  :: !(Maybe Assertion)
                    }

-- | A tree of operations.
type OpTree = LabelTree Operation

toBenchmarks :: [OpTree] -> EvalForest
toBenchmarks = mapMaybe (mapMaybeTree (withName (uncurry . Eval) toEval))
  where
    toEval op = case (opBench op, opWeigh op) of
                  (Nothing, Nothing) -> Nothing
                  ops                -> Just ops

toTests :: [OpTree] -> Test
toTests = TestList . mapMaybeForest (withName (~:) opTest) (const (~:))

withName :: (String -> a -> b) -> (Operation -> Maybe a) -> Operation -> Maybe b
withName jn mf op = jn (opName op) <$> mf op

-- -----------------------------------------------------------------------------

-- TODO: does this /really/ need to be in IO?
newtype TestBenchM r
  = TestBenchM { getOpTrees :: ReaderT Int (WriterT [OpTree] IO) r }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | An environment for combining testing and benchmarking.
type TestBench = TestBenchM ()

runTestBenchDepth :: Int -> TestBench -> IO [OpTree]
runTestBenchDepth d = execWriterT . (`runReaderT` d) . getOpTrees

-- | Label a sub-part of a @TestBench@.
collection :: String -> TestBench -> TestBench
collection nm ops = do d   <- getDepth
                       sub <- liftIO (runTestBenchDepth (d+1) ops)
                       singleTree (Branch d nm sub)

treeList :: [OpTree] -> TestBench
treeList = TestBenchM . lift . tell

singleTree :: OpTree -> TestBench
singleTree = treeList . (:[])

getDepth :: TestBenchM Int
getDepth = TestBenchM ask

runTestBench :: TestBench -> IO [OpTree]
runTestBench = runTestBenchDepth 0

-- | Obtain the resulting tests and benchmarks from the specified
--   @TestBench@.
getTestBenches :: TestBench -> IO (Test, EvalForest)
getTestBenches = fmap (toTests &&& toBenchmarks) . runTestBench

-- | Run the specified benchmarks if and only if all tests pass, using
--   a comparison-based format for benchmarking output.
--
--   For more control, use 'getTestBenches'.
testBench :: TestBench -> IO ()
testBench = testBenchWith testBenchConfig

-- | As with 'testBench' but allow specifying a custom default
--   'Config' parameter rather than 'testBenchConfig'.
testBenchWith :: Config -> TestBench -> IO ()
testBenchWith cfg tb = execParser (optionParser cfg) >>= go
  where
    go Version = putStrLn versionInfo >> exitSuccess
    go List    = do
      (_, bf) <- getTestBenches tb
      -- The Config value won't get used, so it's OK just to use the default here.
      evalForest cfg (stripEval bf)
      exitSuccess
    go Run{..} = do
      (tst, bf) <- getTestBenches tb
      testSucc <- if runTests
                     then do tcnts <- runTestTT tst
                             return (errors tcnts == 0 && failures tcnts == 0)
                     else return True
      when (runBench && testSucc) (evalForest benchCfg bf)

    -- To print out the list of benchmarks, we abuse the current
    -- tabular setup for printing results by just disabling all
    -- benchmarks, etc.
    stripEval :: EvalForest -> EvalForest
    stripEval = map (fmap (\e -> Eval (eName e) Nothing Nothing))
    -- Create a new value so that if Eval is expanded we don't
    -- accidentally run something.

-- -----------------------------------------------------------------------------

-- | Create a single benchmark evaluated to normal form, where the
--   results should equal the value specified.
--
--   Will also weigh the function.
nfEq :: (NFData b, Show b, Eq b) => b -> (a -> b) -> String -> a -> TestBench
nfEq = mkTestBench (Just .: nf) (Just .: getWeight) . (Just .: (@=?))

-- | Create a single benchmark evaluated to weak head normal form,
--   where the results should equal the value specified.
whnfEq :: (Show b, Eq b) => b -> (a -> b) -> String -> a -> TestBench
whnfEq = mkTestBench (Just .: whnf) (const (const Nothing)) . (Just .: (@=?))

-- | A way of writing custom testing/benchmarking statements.  You
--   will probably want to use one of the pre-defined versions
--   instead.
mkTestBench :: ((a -> b) -> a -> Maybe Benchmarkable)
                  -- ^ Define the benchmark to be performed, if any.
               -> ((a -> b) -> a -> Maybe GetWeight)
                  -- ^ If a benchmark is performed, should its memory
                  --   usage also be calculated?  See the
                  --   documentation for 'weigh' on how to get this
                  --   work.
               -> (b -> Maybe Assertion)
                  -- ^ Should the result be checked?
               -> (a -> b) -> String -> a -> TestBench
mkTestBench toB w checkRes fn nm arg = do d <- getDepth
                                          singleTree
                                            . Leaf d
                                            $ Op { opName  = nm
                                                 , opBench = toB fn arg
                                                 , opWeigh = w fn arg
                                                 , opTest  = checkRes (fn arg)
                                                 }

--------------------------------------------------------------------------------

-- | Compare how various input values (of the same type) behave for a
--   specific function.
--
--   By default:
--
--   * Results are only evaluated to /Weak Head Normal Form/.  To
--     fully evaluate results, use 'benchNormalForm'.
--
--   * No tests are performed by default; use either 'baseline' or
--     'testWith' to specify one.
compareFunc :: forall params a b. (ProvideParams params (SameAs a) b)
               => String -> (a -> b) -> params
               -> Comparison (SameAs a) b -> TestBench
compareFunc = compareFuncConstraint (Proxy :: Proxy (SameAs a))

-- | As with 'compareFunc' but allow for polymorphic inputs by
--   specifying the constraint to be used.
compareFuncConstraint :: forall params ca b. (ProvideParams params ca b)
                         => Proxy ca -> String -> (forall a. (ca a) => a -> b)
                         -> params -> Comparison ca b -> TestBench
compareFuncConstraint _ lbl f params cmpM = do ops <- liftIO (runComparison ci cmpM)
                                               d   <- getDepth
                                               let opTr = map (Leaf (d+1)) (withOps' ops)
                                               singleTree (Branch d lbl opTr)
  where
    ci0 :: CompInfo ca b
    ci0 = CI { func    = f
             , toBench = Just .: whnf
             , toWeigh = const (const Nothing)
             , toTest  = const Nothing
             }

    params' = toParams params

    ci = appEndo (mkOps params') ci0

    withOps' = appEndo (withOps params' ci)

-- TODO: work out how to fix it if multiple test setting functions are called; might need a Last in here.
-- | Monoidally build up the parameters used to control a 'Comparison'
--   environment.
--
--   This will typically be a combination of 'benchNormalForm' with
--   either 'baseline' or 'testWith'.
data CompParams ca b = CP { withOps :: CompInfo ca b -> Endo [Operation]
                          , mkOps   :: Endo (CompInfo ca b)
                          }

instance Monoid (CompParams ca b) where
  mempty = CP { withOps = mempty
              , mkOps   = mempty
              }

  mappend cp1 cp2 = CP { withOps = mappendBy withOps
                       , mkOps   = mappendBy mkOps
                       }
    where
      mappendBy f = mappend (f cp1) (f cp2)

-- | A convenience class to make it easier to provide 'CompParams'
--   values.
--
--   You can either:
--
--   * Provide no parameters with @mempty@
--
--   * Provide values chained together using @'mappend'@ or @<>@
--
--   * Use the list instance and provide a list of 'CompParams'
--     values.
class ProvideParams cp ca b | cp -> ca b where
  toParams :: cp -> CompParams ca b

instance ProvideParams (CompParams ca b) ca b where
  toParams = id

instance ProvideParams [CompParams ca b] ca b where
  toParams = mconcat

mkOpsFrom :: (CompInfo ca b -> CompInfo ca b) -> CompParams ca b
mkOpsFrom f = mempty { mkOps = Endo f }

-- | Evaluate all benchmarks to normal form.
benchNormalForm :: (NFData b) => CompParams ca b
benchNormalForm = withBenchMode nf

-- | Evaluate all IO-based benchmarks to weak head normal form.
benchIO :: CompParams ca (IO b)
benchIO = withBenchMode (whnfIO .)

-- | Evaluate all IO-based benchmarks to normal form.
benchNormalFormIO :: (NFData b) => CompParams ca (IO b)
benchNormalFormIO = withBenchMode (nfIO .)

-- | Allow specifying how benchmarks should be evaluated.  This may
--   allow usage of methods such as @nfIO@, but this has not been
--   tested as yet.
withBenchMode :: (forall a. (ca a) => (a -> b) -> a -> Benchmarkable) -> CompParams ca b
withBenchMode toB = mkOpsFrom (\ci -> ci { toBench = Just .: toB })

-- | Don't run any benchmarks.  I'm not sure why you'd want to do this
--   as there's surely easier\/better testing environments available,
--   but this way it's possible.
noBenchmarks :: CompParams ca b
noBenchmarks = mkOpsFrom (\ci -> ci { toBench = \_ _ -> Nothing })

-- | Don't run any tests.  This isn't recommended, but could be useful
--   if all you want to do is run comparisons (potentially because no
--   meaningful tests are possible).
noTests :: CompParams ca b
noTests = mkOpsFrom (\ci -> ci { toTest = const Nothing })

-- | Specify a sample baseline value to benchmark and test against
--   (such that the result of applying the function to this @a@ is
--   what everything should match).
--
--   You shouldn't specify this more than once, nor mix it with
--   'noTests' or 'testWith'.
baseline :: (ca a, Eq b, Show b) => String -> a -> CompParams ca b
baseline nm arg = mempty { withOps = addOp
                         , mkOps   = Endo setTest
                         }
  where
    opFrom ci = Op { opName  = nm
                   , opBench = toBench ci (func ci) arg
                   , opWeigh = toWeigh ci (func ci) arg
                   , opTest  = Nothing
                   }

    addOp ci = Endo (opFrom ci:)

    setTest ci = ci { toTest = Just . (func ci arg @=?) }

-- | Specify a predicate that all results should satisfy.
--
--   Note that the last statement between 'testWith', 'baseline' and
--   'noTests' \"wins\" in specifying which testing (if any) to do.
testWith :: (b -> Assertion) -> CompParams ca b
testWith f = mkOpsFrom (\ci -> ci { toTest = Just . f })

-- | Calculate memory usage of the various parameters.
--
--   This requires running your executable with the @-T@ RTS flag.  To
--   do so, you can either do:
--
--   * @my-program +RTS -T -RTS@ (may need to add @-rtsopts@ to your
--     @ghc-options@ in your .cabal file)
--
--   * Add @-rtsopts -with-rtsopts=-T@ to your `ghc-options` field in
--     your .cabal file.
--
--   If this flag is not provided, then this is equivalent to a no-op.
weigh :: (NFData b) => CompParams ca b
weigh = mkOpsFrom (\ci -> ci { toWeigh = Just .: getWeight })

data CompInfo ca b = CI { func    :: (forall a. (ca a) => a -> b)
                        , toBench :: (forall a. (ca a) => (a -> b) -> a -> Maybe Benchmarkable)
                        , toWeigh :: (forall a. (ca a) => (a -> b) -> a -> Maybe GetWeight)
                        , toTest  :: (b -> Maybe Assertion)
                        }

type Comper ca b = ReaderT (CompInfo ca b) (WriterT [Operation] IO)

newtype ComparisonM ca b r = ComparisonM { runComparisonM :: Comper ca b r }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | A specialised monad used solely for running comparisons.
--
--   No lifting is permitted; the only operations permitted are
--   'comp', 'compBench' and 'compTest'.
type Comparison ca b = ComparisonM ca b ()

runComparison :: CompInfo ca b -> Comparison ca b -> IO [Operation]
runComparison cmpr cmpM = execWriterT  . runReaderT (runComparisonM cmpM) $ cmpr

-- | Benchmark and test (if specified) this value against the
--   specified function.
comp :: (ca a) => String -> a -> Comparison ca b
comp = compWith id

-- | Only benchmark (but do not test) this value against the specified
--   function.
compBench :: (ca a) => String -> a -> Comparison ca b
compBench = compWith (\op -> op { opTest = Nothing })

-- | Only test (but do not benchmark) this value against the specified
--   function.
compTest :: (ca a) => String -> a -> Comparison ca b
compTest = compWith (\op -> op { opBench = Nothing })

compWith :: (ca a) => (Operation -> Operation) -> String -> a -> Comparison ca b
compWith f nm arg = ComparisonM $ do ci <- ask
                                     lift $ tell [f (compOp nm arg ci)]

compOp :: (ca a) => String -> a -> CompInfo ca b -> Operation
compOp nm arg ci = Op { opName  = nm
                      , opBench = toBench ci (func ci) arg
                      , opWeigh = toWeigh ci (func ci) arg
                      , opTest  = toTest ci $ func ci arg
                      }

--------------------------------------------------------------------------------

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
