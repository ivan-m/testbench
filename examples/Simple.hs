{-# LANGUAGE ConstraintKinds, FlexibleInstances, MultiParamTypeClasses,
             RankNTypes, TypeFamilies, UndecidableInstances #-}

{- |
   Module      : Main
   Description : Sample TestBench usage
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com



 -}
module Main (main) where

import TestBench

import qualified Data.ByteString      as SB
import qualified Data.ByteString.Lazy as LB
import           Data.Monoid          ((<>))
import           Data.Proxy           (Proxy(..))
import qualified Data.Sequence        as Seq
import           Data.Word            (Word8)
import           Test.HUnit.Base      ((@=?), (@?))

--------------------------------------------------------------------------------

main :: IO ()
main = testBench $ do
  collection "Let's kick this off" $ do
    testOnly 1 length "length of (,)" ('a', ())

    testOnly 0 sum "sum of []" ([] :: [Int])

    -- testOnly False null "Should fail" []

  compareFunc "List length"
              (\n -> length (replicate n ()) == n)
              (testWith (@? "Not as long as specified") <> benchNormalForm)
              (mapM_ (\n -> comp ("len == " ++ show n) n) [1..5])

  compareFuncConstraint (Proxy :: Proxy Sequential)
                        "Length of linear types"
                        len
                        [baseline "Lists" sampleList, normalForm]
                        $ do comp "sequence" (Seq.fromList sampleList)
                             comp "strict bytestring" (SB.pack sampleList)
                             comp "lazy bytestring" (LB.pack sampleList)

  compareFuncAll "Packing and length"
                 (`chooseType` listLength)
                 normalForm

data SequenceType = List
                  | Sequence
                  | StrictBS
                  | LazyBS
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

listLength :: (Sequential l) => Proxy l -> Int
listLength st = len (st `pack` sampleList)

chooseType :: SequenceType -> (forall s. (Sequential s) => Proxy s -> k) -> k
chooseType List      k = k (Proxy :: Proxy [Word8])
chooseType Sequence  k = k (Proxy :: Proxy (Seq.Seq Word8))
chooseType StrictBS  k = k (Proxy :: Proxy SB.ByteString)
chooseType LazyBS    k = k (Proxy :: Proxy LB.ByteString)

sampleList :: [Word8]
sampleList = replicate 1000000 0

class Sequential xs where
  len :: xs -> Int

  pack :: Proxy xs -> [Word8] -> xs

instance Sequential [Word8] where
  len = length

  pack _ = id

instance Sequential (Seq.Seq Word8) where
  len = length

  pack _ = Seq.fromList

instance Sequential SB.ByteString where
  len = SB.length

  pack _ = SB.pack

instance Sequential LB.ByteString where
  len = fromIntegral . LB.length

  pack _ = LB.pack

--------------------------------------------------------------------------------

testOnly :: (Show b, Eq b) => b -> (a -> b) -> String -> a -> TestBench
testOnly = mkTestBench (\_ _ -> Nothing) (\_ _ -> Nothing) . (Just .: (@=?))

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)
