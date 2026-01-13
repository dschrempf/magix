-- |
-- Module      :  Magix.Options
-- Description :  Magical options
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 10:37:48 2024.
module Magix.Options
  ( Verbosity (..),
    Rebuild (..),
    Command (..),
    CleanOptions (..),
    Options (..),
    getCommand,
  )
where

import Control.Applicative (Alternative (..), optional)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Options.Applicative
  ( Parser,
    ParserInfo (infoPolicy),
    command,
    execParser,
    flag,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    infoOption,
    long,
    metavar,
    progDesc,
    short,
    strArgument,
    strOption,
  )
import Options.Applicative.Types (ArgPolicy (..))
import Paths_magix qualified as P

data Verbosity = Info | Debug deriving (Eq, Show)

data Rebuild = ReuseBuildIfAvailable | ForceBuild deriving (Eq, Show)

data CleanOptions = CleanOptions
  { verbosity :: !Verbosity,
    cachePath :: !(Maybe FilePath)
  }
  deriving (Eq, Show, Generic)

data Options = Options
  { verbosity :: !Verbosity,
    forceBuild :: !Rebuild,
    cachePath :: !(Maybe FilePath),
    nixpkgsPath :: !(Maybe FilePath),
    scriptPath :: !FilePath,
    scriptArgs :: ![String]
  }
  deriving (Eq, Show, Generic)

data Command = Clean !CleanOptions | Run !Options

pCommand :: Parser Command
pCommand = pCleanCommand <|> pRun

-- Careful, the command names shadow possible script names.
pCleanCommand :: Parser Command
pCleanCommand =
  hsubparser $
    command "clean-magix-cache" $
      info (Clean <$> pCleanOptions) (progDesc "Clean the Magix cache.")

pCleanOptions :: Parser CleanOptions
pCleanOptions = CleanOptions <$> pLogLevel <*> pCachePath

pRun :: Parser Command
pRun = Run <$> pOptions

pOptions :: Parser Options
pOptions =
  Options
    <$> pLogLevel
    <*> pForceBuild
    <*> pCachePath
    <*> pNixpkgsPath
    <*> pScriptPath
    <*> pScriptArgs

pLogLevel :: Parser Verbosity
pLogLevel =
  flag
    Info
    Debug
    ( long "verbose"
        <> short 'v'
        <> help "Print debug messages"
    )

pForceBuild :: Parser Rebuild
pForceBuild =
  flag
    ReuseBuildIfAvailable
    ForceBuild
    ( long "force-build"
        <> short 'f'
        <> help "Force build, even when cached build exists"
    )

pCachePath :: Parser (Maybe FilePath)
pCachePath =
  optional $
    strOption
      ( metavar "CACHE_PATH"
          <> long "cache-path"
          <> short 'c'
          <> help "Path of cache directory to use for builds (default: '$XDG_CACHE_HOME/magix')"
      )

pNixpkgsPath :: Parser (Maybe FilePath)
pNixpkgsPath =
  optional $
    strOption
      ( metavar "NIXPKGS_PATH"
          <> long "nixpkgs-path"
          <> short 'n'
          <> help "Path of Nixpkgs repository to use (default: extracted from '$NIX_PATH')"
      )

pScriptPath :: Parser FilePath
pScriptPath =
  strArgument
    (metavar "SCRIPT_FILE_PATH" <> help "File path of script to build, cache and run")

pScriptArgs :: Parser [String]
pScriptArgs =
  many $
    strArgument
      ( metavar "SCRIPT_ARGS"
          <> help "Arguments passed on to the script"
      )

commandParser :: ParserInfo Command
commandParser =
  info
    ( infoOption version (long "version" <> help "Show version.")
        <*> helper
        <*> pCommand
    )
    ( fullDesc
        <> progDesc desc
        <> header version
    )
  where
    desc :: String
    desc = "Build, cache, and run possibly compiled scripts with dependencies using the Nix package manager"

    version :: String
    version = "Magix version " ++ showVersion P.version

getCommand :: IO Command
getCommand = execParser (commandParser {infoPolicy = NoIntersperse})
