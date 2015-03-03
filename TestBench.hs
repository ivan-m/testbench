{- |
   Module      : TestBench
   Description : Create tests and benchmarks together
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module TestBench where

import Criterion
import Test.HUnit.Base (Assertion, Test (..), (~:))

import Control.Applicative (liftA2)
import Data.Functor        ((<$>))
import Data.Maybe          (mapMaybe)

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
              Leaf op -> Leaf <$> liftA2 (fmap . (,)) opName f op
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

toTest :: [OpTree] -> Test
toTest = TestList . opForestTo opTest (~:) (~:)
