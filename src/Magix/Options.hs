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
    CleanMode (..),
    CleanOptions (..),
    PrintOptions (..),
    Options (..),
    getCommand,

    -- * Pure core of 'getCommand'
    RawCommand (..),
    RawOptions (..),
    rawCommandParser,
    resolveCommand,
  )
where

import Control.Applicative ((<|>))
import Data.Foldable (Foldable (..))
import Data.Function ((&))
import Data.Text (pack)
import Data.Version (showVersion)
import GHC.Generics (Generic)
import Magix.BuildMode (BuildMode (..))
import Magix.Env
  ( MagixEnv (..),
    NixEnv (..),
    getMagixEnv,
    getNixEnv,
    magixEnvDesc,
    nixEnvDesc,
  )
import Options.Applicative
  ( Parser,
    ParserInfo,
    command,
    execParser,
    flag,
    footerDoc,
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
    noIntersperse,
    optional,
    progDesc,
    short,
    strArgument,
    strOption,
  )
import Options.Applicative.Help (Doc, Pretty (..), fillSep, hardline, indent, vsep)
import Paths_magix qualified as P
import System.Exit (die)

data Verbosity = Info | Debug deriving (Eq, Show)

data Rebuild = ReuseBuildIfAvailable | ForceBuild deriving (Eq, Show)

data CleanMode
  = RemoveAll
  | -- | Remove the slots whose script is gone, and every entry that is no slot.
    RemoveStale
  deriving (Eq, Show)

data CleanOptions = CleanOptions
  { verbosity :: !Verbosity,
    cachePath :: !(Maybe FilePath),
    cleanMode :: !CleanMode
  }
  deriving (Eq, Show, Generic)

-- | Options of printing the Nix expression of a script.
data PrintOptions = PrintOptions
  { verbosity :: !Verbosity,
    scriptPath :: !FilePath
  }
  deriving (Eq, Show, Generic)

data Options = Options
  { verbosity :: !Verbosity,
    forceBuild :: !Rebuild,
    cachePath :: !(Maybe FilePath),
    buildMode :: !(Maybe BuildMode),
    magixEnv :: !(MagixEnv (Maybe String)),
    nixEnv :: !(NixEnv (Maybe String)),
    scriptPath :: !FilePath,
    scriptArgs :: ![String]
  }
  deriving (Eq, Show, Generic)

data Command = Clean !CleanOptions | Print !PrintOptions | Run !Options
  deriving (Eq, Show)

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
  deriving (Eq, Show)

data RawCommand = RawClean !CleanOptions | RawPrint !PrintOptions | RawRun !RawOptions
  deriving (Eq, Show)

-- | The options of every command.
data SharedOptions = SharedOptions
  { verbosity :: !Verbosity,
    cachePath :: !(Maybe FilePath)
  }

-- | The shared options sit outside the alternative, so that they do not commit
-- it to a run: before @clean-magix-cache@ or the script, they apply to either.
pRawCommand :: Parser RawCommand
pRawCommand = (&) <$> pSharedOptions <*> (pCommand <|> pRawOptions)
  where
    pCommand =
      hsubparser $
        command "clean-magix-cache" (info pCleanOptions (progDesc "Clean the Magix cache."))
          <> command
            "print-magix-expression"
            ( info
                pPrintOptions
                (progDesc "Print the Nix expression building the script from the file 'script' next to the expression; build nothing.")
            )

pSharedOptions :: Parser SharedOptions
pSharedOptions = SharedOptions <$> pLogLevel <*> pCachePath

-- | The shared options after @clean-magix-cache@ apply as well; its
-- @--cache-path@ wins over the one before it.
pCleanOptions :: Parser (SharedOptions -> RawCommand)
pCleanOptions = cleanWith <$> pSharedOptions <*> pCleanMode
  where
    cleanWith after mode before =
      RawClean
        CleanOptions
          { verbosity = eitherDebug before.verbosity after.verbosity,
            cachePath = after.cachePath <|> before.cachePath,
            cleanMode = mode
          }

-- | Either position of @--verbose@ applies; @--cache-path@ before the command
-- does nothing, as it touches no cache.
pPrintOptions :: Parser (SharedOptions -> RawCommand)
pPrintOptions = printWith <$> pLogLevel <*> pPrintScriptPath
  where
    printWith verbosity path before =
      RawPrint
        PrintOptions
          { verbosity = eitherDebug before.verbosity verbosity,
            scriptPath = path
          }
    pPrintScriptPath =
      strArgument (metavar "SCRIPT_FILE_PATH" <> help "File path of script to print the Nix expression of")

-- | Debug if either asks for it: the flag before a command, or after it.
eitherDebug :: Verbosity -> Verbosity -> Verbosity
eitherDebug Debug _ = Debug
eitherDebug _ v = v

pCleanMode :: Parser CleanMode
pCleanMode =
  flag
    RemoveAll
    RemoveStale
    ( long "stale"
        <> help "Remove only the builds of scripts that are gone, and entries of an older cache layout"
    )

pRawOptions :: Parser (SharedOptions -> RawCommand)
pRawOptions =
  runWith
    <$> pForceBuild
    <*> pNixpkgsPath
    <*> pNixpkgsRef
    <*> pScriptPath
    <*> pScriptArgs
  where
    runWith rebuild nixpkgsPath nixpkgsRef scriptPath scriptArgs shared =
      RawRun
        RawOptions
          { verbosity = shared.verbosity,
            forceBuild = rebuild,
            cachePath = shared.cachePath,
            nixpkgsPath = nixpkgsPath,
            nixpkgsRef = nixpkgsRef,
            scriptPath = scriptPath,
            scriptArgs = scriptArgs
          }

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
          <> help "Path of Nixpkgs to use (default: found in '$NIX_PATH', see below)"
      )

pNixpkgsRef :: Parser (Maybe String)
pNixpkgsRef =
  optional $
    strOption
      ( metavar "NIXPKGS_REF"
          <> long "nixpkgs-ref"
          <> short 'r'
          <> help "Nixpkgs Flake reference, e.g. 'nixpkgs' or 'github:NixOS/nixpkgs/nixos-unstable'; when set, uses 'nix build' instead of 'nix-build'"
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

magixEnvVarsFooter :: Doc
magixEnvVarsFooter =
  "Magix-specific environment variables:"
    <> hardline
    <> envDescToDoc vars
  where
    vars = fmap formatEnvVarDesc magixEnvDesc

nixEnvVarsFooter :: Doc
nixEnvVarsFooter =
  "Nix environment variables:"
    <> hardline
    <> envDescToDoc vars
  where
    vars = fmap formatEnvVarDesc nixEnvDesc

envDescToDoc :: (Foldable t) => t Doc -> Doc
envDescToDoc = vsep . toList

formatEnvVarDesc :: (String, [String]) -> Doc
formatEnvVarDesc (varName, varDesc) =
  indent 2 (pretty varName)
    <> hardline
    <> indent 4 (vsep $ map formatLine varDesc)
  where
    formatLine = fillSep . map pretty . words

-- | The command line parser. Arguments after the script path go to the script
-- ('noIntersperse'), also those that look like options of Magix.
rawCommandParser :: ParserInfo RawCommand
rawCommandParser =
  info
    ( infoOption version (long "version" <> help "Show version")
        <*> helper
        <*> pRawCommand
    )
    ( fullDesc
        <> noIntersperse
        <> progDesc desc
        <> header version
        <> footerDoc (Just $ magixEnvVarsFooter <> hardline <> nixEnvVarsFooter)
    )
  where
    desc :: String
    desc = "Build, cache, and run possibly compiled scripts with dependencies using the Nix package manager"

    version :: String
    version = "Magix version " ++ showVersion P.version

getCommand :: IO Command
getCommand = do
  rawCmd <- execParser rawCommandParser
  magixEnv <- getMagixEnv
  nixEnv <- getNixEnv
  either die pure $ resolveCommand rawCmd magixEnv nixEnv

-- | Validate the parsed command line against the environment; 'Left' holds
-- the error message.
resolveCommand ::
  RawCommand ->
  MagixEnv (Maybe String) ->
  NixEnv (Maybe String) ->
  Either String Command
resolveCommand (RawClean co) _ _ = Right $ Clean co
resolveCommand (RawPrint po) _ _ = Right $ Print po
resolveCommand (RawRun raw) magixEnv nixEnv = do
  let mRef = raw.nixpkgsRef <|> magixEnv.nixpkgsRef
  buildMode <- case (raw.nixpkgsPath, mRef) of
    (Just _, Just _) -> Left "--nixpkgs-path and --nixpkgs-ref are mutually exclusive"
    (Just p, Nothing) -> Right $ Just (ChannelBuild p)
    (Nothing, Just r) -> Right $ Just (FlakeBuild (pack r))
    (Nothing, Nothing) -> Right Nothing
  pure $
    Run $
      Options
        { verbosity = raw.verbosity,
          forceBuild = raw.forceBuild,
          cachePath = raw.cachePath,
          buildMode = buildMode,
          magixEnv = magixEnv,
          nixEnv = nixEnv,
          scriptPath = raw.scriptPath,
          scriptArgs = raw.scriptArgs
        }
