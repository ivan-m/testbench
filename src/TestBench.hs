{-# LANGUAGE FlexibleContexts, FlexibleInstances, FunctionalDependencies,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses, RecordWildCards
             #-}

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

When run, the output will look something like:

> Cases: 7  Tried: 7  Errors: 0  Failures: 0
>                           Mean    MeanLB    MeanUB    Stddev  StddevLB  StddevUB  OutlierVariance
> List length
>   len == 1            323.8 ns  318.6 ns  335.9 ns  23.86 ns  5.834 ns  40.90 ns              83%
>   len == 2            352.8 ns  349.1 ns  358.1 ns  15.05 ns  11.76 ns  19.62 ns              61%
>   len == 3            372.4 ns  358.4 ns  393.8 ns  62.50 ns  39.83 ns  90.85 ns              96%
>   len == 4            396.3 ns  378.4 ns  419.2 ns  67.83 ns  46.71 ns  94.74 ns              96%
>   len == 5            426.0 ns  407.0 ns  459.5 ns  82.23 ns  53.37 ns  110.2 ns              97%

 -}
module TestBench
  ( -- * Specification and running
    TestBench
  , testBench
  , testBenchWith
  , testBenchConfig

    -- * Grouping
  , collection

    -- * Comparisons
  , compareFunc

    -- ** List of input values
    -- $listbased
  , compareFuncList
  , compareFuncListIO
  , compareFuncList'
  , compareFuncAll
  , compareFuncAllIO
  , compareFuncAll'

    -- ** Comparison parameters
  , CompParams
  , ProvideParams(..)
  , normalForm
  , normalFormIO
    -- *** Control benchmarking
  , benchNormalForm
  , benchIO
  , benchNormalFormIO
  , withBenchMode
  , noBenchmarks

    -- *** Control testing
  , baseline
  , baselineIO
  , testWith
  , noTests

    -- *** Control function weighing
  , weigh
  , weighIO
  , GetWeight
  , getWeight
  , getWeightIO

    -- ** Specify comparisons
  , Comparison
  , comp
  , compBench
  , compTest

    -- ** Lower-level types
  , ComparisonM

    -- * Manual construction of a TestBench
  , getTestBenches
  , Eval(..)
  , EvalTree
  , EvalForest
  , flattenBenchForest
  , evalForest

    -- ** Direct benchmarks\/tests
  , nfEq
  , whnfEq
  , mkTestBench

    -- ** Lower-level types
  , TestBenchM
  , OpTree
  , Operation
  , LabelTree(..)
  , Depth

  ) where

import TestBench.Commands
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
  = TestBenchM { getOpTrees :: ReaderT Depth (WriterT [OpTree] IO) r }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | An environment for combining testing and benchmarking.
type TestBench = TestBenchM ()

runTestBenchDepth :: Depth -> TestBench -> IO [OpTree]
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

getDepth :: TestBenchM Depth
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
testBenchWith cfg tb = do
  (tst, bf) <- getTestBenches tb
  args <- execParser (optionParser cfg)
  case args of
    Version      -> putStrLn versionInfo >> exitSuccess
    List         -> evalForest testBenchConfig (stripEval bf) >> exitSuccess
                    -- Can't use the provided config in case it
                    -- dictates CSV output.
    Weigh ind fp -> weighIndex bf ind >>= writeFile fp . show
    Run {..}     -> do
      testSucc <- if runTests
                     then do tcnts <- runTestTT tst
                             return (errors tcnts == 0 && failures tcnts == 0)
                     else return True
      when (runBench && testSucc) (evalForest benchCfg bf)
  where
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
compareFunc :: (ProvideParams params a b)
               => String -> (a -> b) -> params
               -> Comparison a b -> TestBench
compareFunc lbl f params cmpM = do ops <- liftIO (runComparison ci cmpM)
                                   d   <- getDepth
                                   let opTr = map (Leaf (d+1)) (withOps' ops)
                                   singleTree (Branch d lbl opTr)
  where
    ci0 = CI { func    = f
             , toBench = Just .: whnf
             , toWeigh = const (const Nothing)
             , toTest  = const Nothing
             }

    params' = toParams params

    ci = appEndo (mkOps params') ci0

    withOps' = appEndo (withOps params' ci)

{- $listbased

Rather than manually stating all the arguments - especially if you're
either a) dealing with a few different types or b) repeating all the
possible targets a few times - it can be helpful to instead use an
enum type to indicate all the possible options with a helper type
class to generate all the possible benchmarks.

For example, consider a case where you wish to compare data structures
of @Word8@ values:

> import qualified Data.ByteString      as SB
> import qualified Data.ByteString.Lazy as LB
> import           Data.Monoid          ((<>))
> import           Data.Proxy           (Proxy(..))
> import qualified Data.Sequence        as Seq
>
> -- | All the types we care about.
> data SequenceType = List
>                   | Sequence
>                   | StrictBS
>                   | LazyBS
>   deriving (Eq, Ord, Show, Read, Enum, Bounded)
>
> -- | The function we actually want to benchmark.
> listLength :: (Sequential l) => Proxy l -> Int
> listLength st = len (st `pack` sampleList)
>
> -- | How to run a function on our chosen type.
> chooseType :: SequenceType -> (forall s. (Sequential s) => Proxy s -> k) -> k
> chooseType List      k = k (Proxy :: Proxy [Word8])
> chooseType Sequence  k = k (Proxy :: Proxy (Seq.Seq Word8))
> chooseType StrictBS  k = k (Proxy :: Proxy SB.ByteString)
> chooseType LazyBS    k = k (Proxy :: Proxy LB.ByteString)
>
> sampleList :: [Word8]
> sampleList = replicate 1000000 0
>
> -- | A common type class containing all the functions we want to test.
> class Sequential xs where
>   len :: xs -> Int
>
>   pack :: Proxy xs -> [Word8] -> xs
>
> instance Sequential [Word8] where
>   len = length
>
>   pack _ = id
>
> instance Sequential (Seq.Seq Word8) where
>   len = length
>
>   pack _ = Seq.fromList
>
> instance Sequential SB.ByteString where
>   len = SB.length
>
>   pack _ = SB.pack
>
> instance Sequential LB.ByteString where
>   len = fromIntegral . LB.length
>
>   pack _ = LB.pack

We can then write as our benchmark:

@
'compareFuncAll' "Packing and length"
               (flip chooseType listLength)
               'normalForm'
@

This may seem like a lot of up-front work just to avoid having to
write out all the cases manually, but if you write a lot of similar
benchmarks comparing different aspects of these sequential structures
then the @chooseType@ function ends up being rather trivial to write
(but alas, barring Template Haskell, not possible to easily automate).

Furthermore, you can now be sure that you won't forget a case!

-}

-- | As with 'compareFunc' but use the provided list of values to base
--   the benchmarking off of.
--
--   This is useful in situations where you create an enumeration type
--   to describe everything you're benchmarking and a function that
--   takes one of these values and evaluates it.
--
--   'baseline' is used on the first value (if non-empty); the 'Show'
--   instance is used to provide labels.
compareFuncList :: (ProvideParams params a b, Show a, Eq b, Show b)
                   => String -> (a -> b) -> params
                   -> [a] -> TestBench
compareFuncList = compareFuncListWith baseline

-- | A variant of 'compareFuncList' that allows for the function to
--   return an 'IO' value.
compareFuncListIO :: (ProvideParams params a (IO b), Show a, Eq b, Show b)
                     => String -> (a -> IO b) -> params
                     -> [a] -> TestBench
compareFuncListIO = compareFuncListWith baselineIO

compareFuncListWith :: (ProvideParams params a b, Show a)
                       => (String -> a -> CompParams a b)
                       -> String -> (a -> b) -> params -> [a] -> TestBench
compareFuncListWith bline lbl f params lst =
  case lst of
    []     -> getDepth >>= \d -> singleTree (Branch d lbl [])
    (a:as) -> compareFunc lbl f (bline (show a) a `mappend` toParams params)
                                (mapM_ (comp =<< show) as)

-- | A variant of 'compareFuncList' that doesn't use 'baseline'
--   (allowing you to specify your own test).
compareFuncList' :: (ProvideParams params a b, Show a)
                    => String -> (a -> b) -> params
                    -> [a] -> TestBench
compareFuncList' lbl f params = compareFunc lbl f params . mapM_ (comp =<< show)

-- | An extension to 'compareFuncList' that uses the 'Bounded' and
--   'Enum' instances to generate the list of all values.
compareFuncAll :: (ProvideParams params a b, Show a, Enum a, Bounded a
                  , Eq b, Show b) => String -> (a -> b) -> params -> TestBench
compareFuncAll lbl f params = compareFuncList lbl f params [minBound..maxBound]

-- | An extension to 'compareFuncListIO' that uses the 'Bounded' and
--   'Enum' instances to generate the list of all values.
compareFuncAllIO :: (ProvideParams params a (IO b), Show a, Enum a, Bounded a
                    , Eq b, Show b) => String -> (a -> IO b) -> params -> TestBench
compareFuncAllIO lbl f params = compareFuncListIO lbl f params [minBound..maxBound]

-- | A variant of 'comapreFuncAll' that doesn't use 'baseline'
--   (allowing you to specify your own test).
compareFuncAll' :: (ProvideParams params a b, Show a, Enum a, Bounded a)
                   => String -> (a -> b) -> params -> TestBench
compareFuncAll' lbl f params = compareFuncList' lbl f params [minBound..maxBound]

-- TODO: work out how to fix it if multiple test setting functions are called; might need a Last in here.
-- | Monoidally build up the parameters used to control a 'Comparison'
--   environment.
--
--   This will typically be a combination of 'benchNormalForm' with
--   either 'baseline' or 'testWith'.
data CompParams a b = CP { withOps :: CompInfo a b -> Endo [Operation]
                         , mkOps   :: Endo (CompInfo a b)
                         }

instance Monoid (CompParams a b) where
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
class ProvideParams cp a b | cp -> a b where
  toParams :: cp -> CompParams a b

instance ProvideParams (CompParams a b) a b where
  toParams = id

instance ProvideParams [CompParams a b] a b where
  toParams = mconcat

mkOpsFrom :: (CompInfo a b -> CompInfo a b) -> CompParams a b
mkOpsFrom f = mempty { mkOps = Endo f }

-- | Evaluate all benchmarks to normal form.
benchNormalForm :: (NFData b) => CompParams a b
benchNormalForm = withBenchMode nf

-- | A combination of 'benchNormalForm' and 'weigh', taking into
--   account the common case that you want to consider a value that
--   can - and should - be evaluated to normal form.
normalForm :: (NFData b) => CompParams a b
normalForm = benchNormalForm `mappend` weigh

-- | A variant of 'normalForm' where the results are within @IO@.
normalFormIO :: (NFData b) => CompParams a (IO b)
normalFormIO = benchNormalFormIO `mappend` weighIO

-- | Evaluate all IO-based benchmarks to weak head normal form.
benchIO :: CompParams a (IO b)
benchIO = withBenchMode (whnfIO .)

-- | Evaluate all IO-based benchmarks to normal form.
benchNormalFormIO :: (NFData b) => CompParams a (IO b)
benchNormalFormIO = withBenchMode (nfIO .)

-- | Allow specifying how benchmarks should be evaluated.  This may
--   allow usage of methods such as @nfIO@, but this has not been
--   tested as yet.
withBenchMode :: ((a -> b) -> a -> Benchmarkable) -> CompParams a b
withBenchMode toB = mkOpsFrom (\ci -> ci { toBench = Just .: toB })

-- | Don't run any benchmarks.  I'm not sure why you'd want to do this
--   as there's surely easier\/better testing environments available,
--   but this way it's possible.
noBenchmarks :: CompParams a b
noBenchmarks = mkOpsFrom (\ci -> ci { toBench = \_ _ -> Nothing })

-- | Don't run any tests.  This isn't recommended, but could be useful
--   if all you want to do is run comparisons (potentially because no
--   meaningful tests are possible).
noTests :: CompParams a b
noTests = mkOpsFrom (\ci -> ci { toTest = const Nothing })

-- | Specify a sample baseline value to benchmark and test against
--   (such that the result of applying the function to this @a@ is
--   what everything should match).
--
--   You shouldn't specify this more than once, nor mix it with
--   'noTests' or 'testWith'.
baseline :: (Eq b, Show b) => String -> a -> CompParams a b
baseline = baselineWith (@=?)

-- | A variant of 'baseline' where the function returns an 'IO' value.
baselineIO :: (Eq b, Show b) => String -> a -> CompParams a (IO b)
baselineIO = baselineWith (liftA2' (@=?))
  where
    liftA2' f ma mb = do a <- ma
                         b <- mb
                         f a b

baselineWith :: (b -> b -> Assertion) -> String -> a -> CompParams a b
baselineWith mkAssert nm arg = mempty { withOps = addOp
                                      , mkOps   = Endo setTest
                                      }
  where
    opFrom ci = Op { opName  = nm
                   , opBench = toBench ci (func ci) arg
                   , opWeigh = toWeigh ci (func ci) arg
                   , opTest  = Nothing
                   }

    addOp ci = Endo (opFrom ci:)

    setTest ci = ci { toTest = Just . mkAssert (func ci arg) }

-- | Specify a predicate that all results should satisfy.
--
--   Note that the last statement between 'testWith', 'baseline' and
--   'noTests' \"wins\" in specifying which testing (if any) to do.
testWith :: (b -> Assertion) -> CompParams a b
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
weigh :: (NFData b) => CompParams a b
weigh = mkOpsFrom (\ci -> ci { toWeigh = Just .: getWeight })

-- | An IO-based equivalent to 'weigh'
weighIO :: (NFData b) => CompParams a (IO b)
weighIO = mkOpsFrom (\ci -> ci { toWeigh = Just .: getWeightIO })

data CompInfo a b = CI { func    :: a -> b
                       , toBench :: (a -> b) -> a -> Maybe Benchmarkable
                       , toWeigh :: (a -> b) -> a -> Maybe GetWeight
                       , toTest  :: b -> Maybe Assertion
                       }

type Comper a b = ReaderT (CompInfo a b) (WriterT [Operation] IO)

newtype ComparisonM a b r = ComparisonM { runComparisonM :: Comper a b r }
  deriving (Functor, Applicative, Monad, MonadIO)

-- | A specialised monad used solely for running comparisons.
--
--   No lifting is permitted; the only operations permitted are
--   'comp', 'compBench' and 'compTest'.
type Comparison a b = ComparisonM a b ()

runComparison :: CompInfo a b -> Comparison a b -> IO [Operation]
runComparison cmpr cmpM = execWriterT  . runReaderT (runComparisonM cmpM) $ cmpr

-- | Benchmark and test (if specified) this value against the
--   specified function.
comp :: String -> a -> Comparison a b
comp = compWith id

-- | Only benchmark (but do not test) this value against the specified
--   function.
compBench :: String -> a -> Comparison a b
compBench = compWith (\op -> op { opTest = Nothing })

-- | Only test (but do not benchmark) this value against the specified
--   function.
compTest :: String -> a -> Comparison a b
compTest = compWith (\op -> op { opBench = Nothing, opWeigh = Nothing })

compWith :: (Operation -> Operation) -> String -> a -> Comparison a b
compWith f nm arg = ComparisonM $ do ci <- ask
                                     lift $ tell [f (compOp nm arg ci)]

compOp :: String -> a -> CompInfo a b -> Operation
compOp nm arg ci = Op { opName  = nm
                      , opBench = toBench ci (func ci) arg
                      , opWeigh = toWeigh ci (func ci) arg
                      , opTest  = toTest ci $ func ci arg
                      }

--------------------------------------------------------------------------------

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
