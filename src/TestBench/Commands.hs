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
  , resetUnusedConfig
  , versionInfo
  , parseWith
  , configParser
  , weighIndexArg
  , weighFileArg
  ) where

import Criterion.Main.Options (config, defaultConfig)
import Criterion.Types        (Config(..), Verbosity(Quiet))

import Options.Applicative.Builder (auto, footer, fullDesc, header, help, info,
                                    internal, long, option, short, strOption,
                                    switch)
import Options.Applicative.Extra   (helper)
import Options.Applicative.Types   (Parser, ParserInfo)

import Control.Applicative ((<|>))
import Data.Monoid         ((<>))
import Data.Version        (showVersion)
import Paths_testbench     (version)

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
  <> footer (unwords [ "Most of these options are for Criterion; see it for more information."
                     , "However, not all are used: CSV is the only allowed output file, and"
                     , "verbosity is always set to Quiet."
                     ])

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

-- | This is the same as 'defaultConfig' from criterion but with the
--   verbosity set to 'Quiet' to avoid unnecessary noise on stdout.
--
--   @since 0.2.0.0
testBenchConfig :: Config
testBenchConfig = resetUnusedConfig defaultConfig

resetUnusedConfig :: Config -> Config
resetUnusedConfig cfg = cfg { rawDataFile = Nothing
                            , reportFile  = Nothing
                            , jsonFile    = Nothing
                            , junitFile   = Nothing
                            , verbosity   = Quiet
                            }

-- This is based upon Criterion.Main.Options

configParser :: Config -> Parser Config
configParser = config . resetUnusedConfig

weighIndexArg :: String
weighIndexArg = "only-weigh-this-index"

weighFileArg :: String
weighFileArg = "write-weigh-results-to"
