{-# LANGUAGE ConstraintKinds, FlexibleInstances, MultiParamTypeClasses,
             TypeFamilies, UndecidableInstances #-}

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
import           Data.Proxy           (Proxy (..))
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
                        "Length of different linear types"
                        len
                        (baseline "Lists" sampleList <> benchNormalForm)
                        $ do comp "sequence" (Seq.fromList sampleList)
                             comp "strict bytestring" (SB.pack sampleList)
                             comp "lazy bytestring" (LB.pack sampleList)

sampleList :: [Word8]
sampleList = [1..10]

class Sequential xs where
  len :: xs -> Int

instance Sequential [Word8] where
  len = length

instance Sequential (Seq.Seq Word8) where
  len = length

instance Sequential SB.ByteString where
  len = SB.length

instance Sequential LB.ByteString where
  len = fromIntegral . LB.length

--------------------------------------------------------------------------------

testOnly :: (Show b, Eq b) => b -> (a -> b) -> String -> a -> TestBench
testOnly = mkTestBench (\_ _ -> Nothing) . (Just .: (@=?))

(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)