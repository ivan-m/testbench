{-# LANGUAGE CPP, ConstraintKinds, FlexibleInstances,
             GeneralizedNewtypeDeriving, MultiParamTypeClasses, RankNTypes,
             ScopedTypeVariables, UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

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
>               (testWith (@? "Not as long as specified") `mappend` benchNormalForm)
>               (mapM_ (\n -> comp ("len == " ++ show n) n) [1..5])
>   -- Polymorphic comparisons.
>   --
>   -- Currently it isn't possible to use a Proxy as the argument to
>   -- the function (this will probably require Injective Type Familes
>   -- in GHC 8.0), so we're using 'undefined' to specify the type.
>   compareFuncConstraint (Proxy :: Proxy (CUnion Eq Num))
>                         "Number type equality"
>                         (join (==) . (0`asTypeOf`))
>                         (baseline "Integer" (undefined :: Integer) `mappend` benchNormalForm)
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
    -- ** Running manually
  , getTestBenches
  , BenchTree
  , BenchForest
  , flattenBenchForest
  , benchmarkForest
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
  , withBenchMode
  , noBenchmarks

    -- *** Control testing
  , baseline
  , testWith
  , noTests

    -- ** Specify comparisons
  , comp
  , compBench
  , compTest

    -- ** Lower-level types
  , ComparisonM
  , SameAs
  ) where

import Criterion.Tree
import TestBench.LabelTree

import Criterion              (Benchmarkable, nf, whnf)
import Criterion.Main.Options (defaultConfig)
import Test.HUnit.Base        (Assertion, Counts (..), Test (..), (@=?), (~:))
import Test.HUnit.Text        (runTestTT)

import Control.Applicative             (liftA2)
import Control.Arrow                   ((&&&))
import Control.DeepSeq                 (NFData (..))
import Control.Monad                   (when)
import Control.Monad.IO.Class          (MonadIO (liftIO))
import Control.Monad.Trans.Class       (lift)
import Control.Monad.Trans.Reader      (ReaderT, ask, runReaderT)
import Control.Monad.Trans.Writer.Lazy (WriterT, execWriterT, tell)
import Data.Maybe                      (mapMaybe)
import Data.Monoid                     (Endo (..))
import Data.Proxy                      (Proxy (..))

-- -----------------------------------------------------------------------------

-- | An individual operation potentially consisting of a benchmark
--   and/or test.
data Operation = Op { opName  :: String
                    , opBench :: Maybe Benchmarkable
                    , opTest  :: Maybe Assertion
                    }

-- | A tree of operations.
type OpTree = LabelTree Operation

opTreeTo :: (Operation -> Maybe a) -> OpTree -> Maybe (LabelTree (String, a))
opTreeTo f = go
  where
    go tr = case tr of
              Leaf op       -> Leaf <$> liftA2 (fmap . (,)) opName f op
              Branch lb trs -> case mapMaybe go trs of
                                 []   -> Nothing
                                 trs' -> Just (Branch lb trs')

opForestTo :: (Operation -> Maybe a) -> (String -> a -> b) -> (String -> [b] -> b)
              -> [OpTree] -> [b]
opForestTo f lf br = mapMaybe (fmap (toCustomTree (uncurry lf) br) . opTreeTo f)

toBenchmarks :: [OpTree] -> BenchForest
toBenchmarks = mapMaybe (opTreeTo opBench)

toTests :: [OpTree] -> Test
toTests = TestList . opForestTo opTest (~:) (~:)

-- -----------------------------------------------------------------------------

-- TODO: does this /really/ need to be in IO?
newtype TestBenchM r = TestBenchM { getOpTrees :: WriterT [OpTree] IO r}
                     deriving (Functor, Applicative, Monad, MonadIO)

type TestBench = TestBenchM ()

makeOpTree :: String -> TestBench -> IO OpTree
makeOpTree nm = fmap (Branch nm) . execWriterT . getOpTrees

-- | Label a sub-part of a @TestBench@.
collection :: String -> TestBench -> TestBench
collection nm ops = liftIO (makeOpTree nm ops) >>= singleTree

treeList :: [OpTree] -> TestBench
treeList = TestBenchM . tell

singleTree :: OpTree -> TestBench
singleTree = treeList . (:[])

runTestBench :: TestBench -> IO [OpTree]
runTestBench = execWriterT . getOpTrees

-- | Obtain the resulting test and benchmarks from the specified
--   @TestBench@.
getTestBenches :: TestBench -> IO (Test, BenchForest)
getTestBenches = fmap (toTests &&& toBenchmarks) . runTestBench

-- | Run the specified benchmarks if and only if all tests pass.
testBench :: TestBench -> IO ()
testBench tb = do (tst,bf) <- getTestBenches tb
                  tcnts <- runTestTT tst
                  when (errors tcnts == 0 && failures tcnts == 0)
                       (benchmarkForest defaultConfig bf) -- TODO: make this configurable

-- -----------------------------------------------------------------------------

nfEq :: (NFData b, Show b, Eq b) => b -> (a -> b) -> String -> a -> TestBench
nfEq = mkTestBench (Just .: nf) . (Just .: (@=?))

whnfEq :: (Show b, Eq b) => b -> (a -> b) -> String -> a -> TestBench
whnfEq = mkTestBench (Just .: whnf) . (Just .: (@=?))

-- | A way of writing custom testing/benchmarking statements.  You
--   will probably want to use one of the pre-defined versions
--   instead.
mkTestBench :: ((a -> b) -> a -> Maybe Benchmarkable) -> (b -> Maybe Assertion)
               -> (a -> b) -> String -> a -> TestBench
mkTestBench toB checkRes fn nm arg = singleTree
                                     . Leaf
                                     $ Op { opName  = nm
                                          , opBench = toB fn arg
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
compareFunc :: forall a b. String -> (a -> b) -> CompParams (SameAs a) b
               -> Comparison (SameAs a) b -> TestBench
compareFunc = compareFuncConstraint (Proxy :: Proxy (SameAs a))

-- | An alias for readability.
type SameAs a = (~) a

-- | As with 'compareFunc' but allow for polymorphic inputs by
--   specifying the constraint to be used.
compareFuncConstraint :: forall ca b. Proxy ca -> String -> (forall a. (ca a) => a -> b)
                         -> CompParams ca b -> Comparison ca b -> TestBench
compareFuncConstraint _ lbl f params cmpM = do ops <- liftIO (runComparison ci cmpM)
                                               let opTr = map Leaf (withOps' ops)
                                               singleTree (Branch lbl opTr)
  where
    ci0 :: CompInfo ca b
    ci0 = CI { func    = f
             , toBench = Just .: whnf
             , toTest  = const Nothing
             }

    (withOps , Endo mkCI) = unCP params
    ci = mkCI ci0

    withOps' = appEndo (withOps ci)

-- TODO: work out how to fix it if multiple test setting functions are called; might need a Last in here.
newtype CompParams ca b = CP { unCP :: ( CompInfo ca b -> Endo [Operation]
                                       , Endo (CompInfo ca b)
                                       )
                             }
                        deriving (Monoid)

benchNormalForm :: (NFData b) => CompParams ca b
benchNormalForm = withBenchMode nf

withBenchMode :: (forall a. (ca a) => (a -> b) -> a -> Benchmarkable) -> CompParams ca b
withBenchMode toB = CP (mempty, Endo (\ci -> ci { toBench = Just .: toB }))

noBenchmarks :: CompParams ca b
noBenchmarks = CP (mempty, Endo (\ci -> ci { toBench = \_ _ -> Nothing }))

noTests :: CompParams ca b
noTests = CP (mempty, Endo (\ci -> ci { toTest = const Nothing }))

-- | Specify a sample baseline value to benchmark and test against
--   (such that the result of applying the function to this @a@ is
--   what everything should match).
--
--   You shouldn't specify this more than once, nor mix it with
--   'noTests' or 'testWith'.
baseline :: (ca a, Eq b, Show b) => String -> a -> CompParams ca b
baseline nm arg = CP (addOp, Endo setTest)
  where
    opFrom ci = Op { opName  = nm
                   , opBench = toBench ci (func ci) arg
                   , opTest  = Nothing
                   }

    addOp ci = Endo (opFrom ci:)

    setTest ci = ci { toTest = Just . (func ci arg @=?) }

-- | Specify a predicate that all results should satisfy.
--
--   Note that the last statement between 'testWith', 'baseline' and
--   'noTests' \"wins\" in specifying which testing (if any) to do.
testWith :: (b -> Assertion) -> CompParams ca b
testWith f = CP (mempty, Endo (\ci -> ci { toTest = Just . f }))

data CompInfo ca b = CI { func    :: (forall a. (ca a) => a -> b)
                        , toBench :: (forall a. (ca a) => (a -> b) -> a -> Maybe Benchmarkable)
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
                      , opTest  = toTest ci $ func ci arg
                      }

-- | The union of two @(* -> 'Constraint')@ values.
--
--   Whilst @type EqNum a = ('Eq' a, 'Num' a)@ is a valid
--   specificatoin of a 'Constraint' when using the @ConstraintKinds@
--   extension, it cannot be used with 'compareFuncConstraint' as type
--   aliases cannot be partially applied.
--
--   As such, you can use @type EqNum = CUnion Eq Num@ instead.
class (c1 a, c2 a) => CUnion c1 c2 a
instance (c1 a, c2 a) => CUnion c1 c2 a

--------------------------------------------------------------------------------

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
