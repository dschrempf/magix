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

import Data.Text (pack)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Magix.BuildMode (BuildMode (..))
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
    many,
    metavar,
    optional,
    progDesc,
    short,
    strArgument,
    strOption,
    (<|>),
  )
import Options.Applicative.Types (ArgPolicy (..))
import Paths_magix qualified as P
import System.Environment (lookupEnv)
import System.Exit (die)

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
    -- | Either a Nixpkgs channel path or a flake reference; 'Nothing' means use NIX_PATH.
    buildMode :: !(Maybe BuildMode),
    scriptPath :: !FilePath,
    scriptArgs :: ![String]
  }
  deriving (Eq, Show, Generic)

data Command = Clean !CleanOptions | Run !Options

-- | Raw options as parsed from the command line, before validation.
data RawOptions = RawOptions
  { verbosity :: !Verbosity,
    forceBuild :: !Rebuild,
    cachePath :: !(Maybe FilePath),
    nixpkgsPath :: !(Maybe FilePath),
    nixpkgsRef :: !(Maybe String),
    scriptPath :: !FilePath,
    scriptArgs :: ![String]
  }

data RawCommand = RawClean !CleanOptions | RawRun !RawOptions

pRawCommand :: Parser RawCommand
pRawCommand = pCleanCommand <|> pRawRun
  where
    pCleanCommand =
      hsubparser $
        command "clean-magix-cache" $
          info (RawClean <$> pCleanOptions) (progDesc "Clean the Magix cache.")
    pRawRun = RawRun <$> pRawOptions

pCleanOptions :: Parser CleanOptions
pCleanOptions = CleanOptions <$> pLogLevel <*> pCachePath

pRawOptions :: Parser RawOptions
pRawOptions =
  RawOptions
    <$> pLogLevel
    <*> pForceBuild
    <*> pCachePath
    <*> pNixpkgsPath
    <*> pNixpkgsRef
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
          <> help "Path of Nixpkgs channel to use (default: extracted from '$NIX_PATH')"
      )

pNixpkgsRef :: Parser (Maybe String)
pNixpkgsRef =
  optional $
    strOption
      ( metavar "NIXPKGS_REF"
          <> long "nixpkgs-ref"
          <> short 'r'
          <> help "Nixpkgs flake reference, e.g. 'nixpkgs' or 'github:NixOS/nixpkgs/nixos-unstable'. Also read from MAGIX_NIXPKGS_REF. When set, uses 'nix build' instead of 'nix-build'."
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

rawCommandParser :: ParserInfo RawCommand
rawCommandParser =
  info
    ( infoOption version (long "version" <> help "Show version.")
        <*> helper
        <*> pRawCommand
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
getCommand = do
  rawCmd <- execParser (rawCommandParser {infoPolicy = NoIntersperse})
  case rawCmd of
    RawClean co ->
      pure $ Clean co
    RawRun raw -> do
      envRef <- lookupEnv "MAGIX_NIXPKGS_REF"
      let mRef = raw.nixpkgsRef <> envRef
      buildMode <- case (raw.nixpkgsPath, mRef) of
        (Just _, Just _) -> die "--nixpkgs-path and --nixpkgs-ref are mutually exclusive"
        (Just p, Nothing) -> pure $ Just (ChannelBuild p)
        (Nothing, Just r) -> pure $ Just (FlakeBuild (pack r))
        (Nothing, Nothing) -> pure Nothing
      pure $
        Run $
          Options
            { verbosity = raw.verbosity,
              forceBuild = raw.forceBuild,
              cachePath = raw.cachePath,
              buildMode = buildMode,
              scriptPath = raw.scriptPath,
              scriptArgs = raw.scriptArgs
            }
