{-# LANGUAGE ConstraintKinds, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, RankNTypes, ScopedTypeVariables,
             UndecidableInstances #-}

{- |
   Module      : TestBench
   Description : Create tests and benchmarks together
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



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
