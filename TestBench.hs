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

type OpTreeM a = LabelTree Operation

type OpTree = OpTreeM ()

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

newtype OpM a r = OpM { getOpTree :: WriterT [OpTreeM a] IO r}
                deriving (Functor, Applicative, Monad, MonadIO)

type Op = OpM () ()

makeOpTree :: String -> OpM a r -> IO (OpTreeM a)
makeOpTree nm = fmap (Branch nm) . execWriterT . getOpTree

collection :: String -> OpM a r -> OpM a ()
collection nm ops = liftIO (makeOpTree nm ops) >>= singleTree

treeList :: [OpTreeM a] -> OpM a ()
treeList = OpM . tell

singleTree :: OpTreeM a -> OpM a ()
singleTree = treeList . (:[])

-- -----------------------------------------------------------------------------

{-

Not sure if `b' can be generic (ideally it would be that given `a' you know `b', but I don't think that will pan out).

Ultimately, the function should be something along the lines of:

(BConstraints b) => (forall a. (AConstraints a) => a -> b)

-}

-- data TBOp = TBO { tboName :: !String
--                 , tboFunction :: a -> b
--                 , tbAConstraint :: Proxy (* -> Constraint)
--                 , tbBConstraint :: Proxy (* -> Constraint)
--                 }


-- For benchmarking:
--
-- * Name
-- * Function (a -> b)
-- * Argument of type a
-- * Whether to do it to NF or WHNF (maybe with IO as well)

-- For testing:
--
-- * Name
-- * Some expression that evaluates to a Bool (or 2 args)

nfEq :: (NFData b, Show b, Eq b) => b -> (a -> b) -> String -> a -> Op
nfEq = mkOp nf . (==)

whnfEq :: (Show b, Eq b) => b -> (a -> b) -> String -> a -> Op
whnfEq = mkOp whnf . (==)

mkOp :: (Show b) => ((a -> b) -> a -> Benchmarkable) -> (b -> Bool) -> (a -> b) -> String -> a -> Op
mkOp toB checkRes fn nm arg = singleTree . Leaf $ Op { opName  = nm
                                                     , opBench = Just (toB fn arg)
                                                     , opTest  = Just (checkRes res @? msg)
                                                     }
  where
    res = fn arg
    msg = "Result value of " ++ show res ++ " does not satisfy predicate."

cpsTest :: (a -> String) -> ((a -> IO ()) -> IO ()) -> IO ()
cpsTest pr mfunc = mfunc (putStrLn . pr)
