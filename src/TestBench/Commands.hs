{-# LANGUAGE RecordWildCards #-}

{- |
   Module      : TestBench.Commands
   Description : Command-line parsing
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

   You only need to use this module if you want to consider custom
   running\/extensions of this library.

 -}
module TestBench.Commands
  ( RunTestBench(..)
  , optionParser
  , testBenchConfig
  , versionInfo
  , parseWith
  , configParser
  , weighIndexArg
  , weighFileArg
  ) where

import Criterion.Analysis     (validateAccessors)
import Criterion.Main.Options (defaultConfig)
import Criterion.Types        (Config(..), Verbosity(Quiet))

import Options.Applicative
import Options.Applicative.Types (readerAsk)

import Control.Monad   (when)
import Data.Char       (isSpace)
import Data.Monoid     ((<>))
import Data.Version    (showVersion)
import Paths_testbench (version)

--------------------------------------------------------------------------------

data RunTestBench = Version
                  | List
                  | Weigh !Int !FilePath
                  | Run { runTests :: !Bool
                        , runBench :: !Bool
                        , benchCfg :: !Config
                        }
  deriving (Eq, Show, Read)

optionParser :: Config -> ParserInfo RunTestBench
optionParser cfg = info (helper <*> parseWith cfg) $
     header versionInfo
  <> fullDesc
  <> footer "Most of these options are for Criterion; see it for more information."

parseWith :: Config -> Parser RunTestBench
parseWith cfg =
      runParse
  <|> Version <$  switch (long "version" <> short 'V' <> help "Show version information")
  <|> List    <$  switch (long "list" <> short 'l' <> help "List all benchmarks")
  <|> Weigh   <$> option auto (long weighIndexArg <> internal) -- Hidden options!
              <*> strOption (long weighFileArg <> internal)
  where
    runParse = Run <$> (not <$> switch (long "no-tests" <> help "Don't run tests"))
                   <*> (not <$> switch (long "no-bench" <> help "Don't run benchmarks"))
                   <*> configParser cfg

versionInfo :: String
versionInfo = "TestBench - " ++ showVersion version

testBenchConfig :: Config
testBenchConfig = defaultConfig { verbosity = Quiet }

-- This is based upon Criterion.Main.Options

configParser :: Config -> Parser Config
configParser Config{..} = Config
  <$> option (range 0.001 0.999)
      (long "ci" <> short 'I' <> metavar "CI" <> value confInterval <>
       help "Confidence interval")
  <*> (not <$> switch (long "no-gc" <> short 'G' <>
                       help "Do not collect garbage between iterations"))
  <*> option (range 0.1 86400)
      (long "time-limit" <> short 'L' <> metavar "SECS" <> value timeLimit <>
       help "Time limit to run a benchmark")
  <*> option (range 1 1000000)
      (long "resamples" <> metavar "COUNT" <> value resamples <>
       help "Number of bootstrap resamples to perform")
  <*> many (option regressParams
            (long "regress" <> metavar "RESP:PRED.." <>
             help "Regressions to perform"))
  -- We don't actually write or record anything to file, so don't
  -- bother asking for these as inputs.
  <*> pure Nothing -- rawDataFile
  <*> pure Nothing -- reportFile
  <*> optional (strOption (long "csv" <> metavar "FILE" <> help "File to write CSV summary to" <>
                           maybe mempty value csvFile))
  <*> pure Nothing -- jsonFile
  <*> pure Nothing -- junitFile
  <*> pure verbosity -- Doesn't change
  <*> pure template  -- Not actually used

range :: (Show a, Read a, Ord a) => a -> a -> ReadM a
range lo hi = do
  s <- readerAsk
  case reads s of
    [(i, "")]
      | i >= lo && i <= hi -> return i
      | otherwise -> readerError $ show i ++ " is outside range " ++
                                   show (lo,hi)
    _             -> readerError $ show s ++ " is not a number"

regressParams :: ReadM ([String], String)
regressParams = do
  m <- readerAsk
  let repl ',' = ' '
      repl c   = c
      tidy       = reverse . dropWhile isSpace . reverse . dropWhile isSpace
      (r,ps)     = break (==':') m
  when (null r) $
    readerError "no responder specified"
  when (null ps) $
    readerError "no predictors specified"
  let ret = (words . map repl . drop 1 $ ps, tidy r)
  either readerError (const (return ret)) $ uncurry validateAccessors ret

weighIndexArg :: String
weighIndexArg = "only-weigh-this-index"

weighFileArg :: String
weighFileArg = "write-weigh-results-to"
