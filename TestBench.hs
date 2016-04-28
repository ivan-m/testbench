{-# LANGUAGE ConstraintKinds, GADTs, GeneralizedNewtypeDeriving,
             NoMonoLocalBinds, RankNTypes #-}
{- |
   Module      : TestBench
   Description : Create tests and benchmarks together
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module TestBench where

import Criterion
import Test.HUnit.Base (Assertion, Test (..), (@?), (@?=), (~:))

import Control.Applicative             (liftA2)
import Control.DeepSeq                 (NFData (..))
import Control.Monad.IO.Class
import Control.Monad.Trans.Writer.Lazy
import Data.Functor                    ((<$>))
import Data.Functor.Identity
import Data.Maybe                      (mapMaybe)
import Data.Proxy

-- -----------------------------------------------------------------------------

data Operation = Op { opName  :: String
                    , opBench :: Maybe Benchmarkable
                    , opTest  :: Maybe Assertion
                    }

data LabelTree a = Leaf a
                 | Branch String [LabelTree a]
                   deriving (Eq, Ord, Show, Read)

type OpTree = LabelTree Operation

opTreeTo :: (Operation -> Maybe a) -> OpTree -> Maybe (LabelTree (String, a))
opTreeTo f = go
  where
    go tr = case tr of
              Leaf op       -> Leaf <$> liftA2 (fmap . (,)) opName f op
              Branch lb trs -> case mapMaybe go trs of
                                 []   -> Nothing
                                 trs' -> Just (Branch lb trs')

toCustomTree :: (String -> a -> b) -> (String -> [b] -> b) -> LabelTree (String,a) -> b
toCustomTree lf br = go
  where
    go tr = case tr of
              Leaf (str,a)   -> lf str a
              Branch str trs -> br str (map go trs)

opForestTo :: (Operation -> Maybe a) -> (String -> a -> b) -> (String -> [b] -> b)
              -> [OpTree] -> [b]
opForestTo f lf br = mapMaybe (fmap (toCustomTree lf br) . opTreeTo f)

toBenchmarks :: [OpTree] -> [Benchmark]
toBenchmarks = opForestTo opBench bench bgroup

toTests :: [OpTree] -> Test
toTests = TestList . opForestTo opTest (~:) (~:)

-- -----------------------------------------------------------------------------

newtype TestBenchM r = TestBenchM { getOpTree :: WriterT [OpTree] IO r}
                     deriving (Functor, Applicative, Monad, MonadIO)

type TestBench = TestBenchM ()

makeOpTree :: String -> TestBench -> IO OpTree
makeOpTree nm = fmap (Branch nm) . execWriterT . getOpTree

collection :: String -> TestBench -> TestBench
collection nm ops = liftIO (makeOpTree nm ops) >>= singleTree

treeList :: [OpTree] -> TestBench
treeList = TestBenchM . tell

singleTree :: OpTree -> TestBench
singleTree = treeList . (:[])

-- -----------------------------------------------------------------------------

nfEq :: (NFData b, Show b, Eq b) => b -> (a -> b) -> String -> a -> TestBench
nfEq = mkOp nf . (==)

whnfEq :: (Show b, Eq b) => b -> (a -> b) -> String -> a -> TestBench
whnfEq = mkOp whnf . (==)

mkOp :: (Show b) => ((a -> b) -> a -> Benchmarkable) -> (b -> Bool) -> (a -> b) -> String -> a -> TestBench
mkOp toB checkRes fn nm arg = singleTree . Leaf $ Op { opName  = nm
                                                     , opBench = Just (toB fn arg)
                                                     , opTest  = Just (checkRes res @? msg)
                                                     }
  where
    res = fn arg
    msg = "Result value of " ++ show res ++ " does not satisfy predicate."

--------------------------------------------------------------------------------







--

--




