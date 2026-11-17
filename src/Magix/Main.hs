-- |
-- Module      :  Magix.Main
-- Description :  Build or reuse the script, and clean the cache
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:12:06 2024.
module Magix.Main
  ( runMagix,
    getScriptExpression,
    cleanCache,
  )
where

import Control.Exception (displayException, throwIO, try)
import Data.ByteString (ByteString, readFile)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8')
import GHC.IO.Exception (IOException (..))
import Magix.Build (BuildStatus (..), build, getBuildStatus, resolveResult, withBuildLock)
import Magix.Cache (removeCacheContents, removeStaleEntries)
import Magix.Config (Config (..), getCacheDir, getConfig, getScriptName)
import Magix.Directives (Directives (..), getDirectives)
import Magix.Expression (getNixExpression)
import Magix.Language.Directives (LanguageDirectives)
import Magix.Options (CleanMode (..), CleanOptions (..), Options (..), PrintOptions (..), Rebuild (..))
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Log.Logger (Logger, Priority (..), logL)
import Prelude hiding (readFile)

newBuild :: Logger -> Config -> LanguageDirectives -> IO ()
newBuild logger conf dirs = do
  let logD = logL logger DEBUG
  logD "Getting Magix Nix expression"
  expr <- getNixExpression conf.scriptName dirs
  logD $ "Magix Nix expression:\n" <> unpack expr
  logD "Building Magix Nix expression"
  build logger conf expr
  logD "Built Magix Nix expression"

-- | Clean the cache.
cleanCache :: Logger -> CleanOptions -> IO ()
cleanCache logger opts = do
  let logI = logL logger INFO
  cacheDir <- getCacheDir opts.cachePath
  case opts.cleanMode of
    RemoveAll -> do
      logI $ "Cleaning Magix cache: " <> cacheDir
      removeCacheContents logger cacheDir
    RemoveStale -> do
      logI $ "Removing stale entries from Magix cache: " <> cacheDir
      removeStaleEntries logger cacheDir
  logI "Done"

data Script = Script
  { contents :: !ByteString,
    directives :: !Directives
  }

-- | Read the script, and parse its directives.
--
-- Log the reason and exit with failure if the script cannot be read; throw
-- if it cannot be decoded, or its directives cannot be parsed.
readScript :: Logger -> FilePath -> IO Script
readScript logger path = do
  let logD = logL logger DEBUG
      logE = logL logger ERROR
  logD $ "Reading script at path: " <> path
  scriptBytes <-
    try (readFile path) >>= \case
      Left (e :: IOException) -> do
        -- Not every IOException names the path, so drop it and name it here.
        let reason = displayException e {ioe_filename = Nothing}
        logE $ "Could not read file: " <> path <> ": " <> reason
        exitFailure
      Right bytes -> pure bytes

  logD "Decoding script file"
  scriptTxt <- case decodeUtf8' scriptBytes of
    Left e -> logE "Failed decoding script file" >> throwIO e
    Right t -> logD "Successfully decoded script file" >> pure t

  logD "Parsing directives"
  directives <- case getDirectives path scriptTxt of
    Left e -> logE "Failed parsing directives" >> throwIO e
    Right result -> do
      logD $ "Successfully parsed directives: " <> show result
      pure result
  pure $ Script scriptBytes directives

-- | The Nix expression building the script; see 'getNixExpression'. Build
-- nothing, and touch no cache. A @#!nixpkgs@ directive does not enter the
-- expression: it takes the Nixpkgs as argument.
--
-- Fail like 'readScript'.
getScriptExpression :: Logger -> PrintOptions -> IO Text
getScriptExpression logger opts = do
  logL logger DEBUG $ "Options are: " <> show opts
  script <- readScript logger opts.scriptPath
  getNixExpression (getScriptName opts.scriptPath) script.directives.language

-- | Build the script, or reuse its build, and return its executable.
--
-- Fail like 'readScript'.
runMagix :: Logger -> Options -> IO FilePath
runMagix logger opts = do
  let logD = logL logger DEBUG
      logI = logL logger INFO
  logD $ "Options are: " <> show opts

  script <- readScript logger opts.scriptPath
  let directives = script.directives

  conf <- getConfig opts script.contents directives.nixpkgsRef
  logD $ "Magix configuration is " <> show conf

  result <- withBuildLock logger conf $ do
    case opts.forceBuild of
      ForceBuild -> do
        logI "Forcing build"
        newBuild logger conf directives.language
      ReuseBuildIfAvailable -> do
        logD "Reusing build if available"
        logD "Checking build status"
        buildStatus <- getBuildStatus conf
        case buildStatus of
          HasBeenBuilt ->
            logI "Script has already been built"
          NeedToBuild ->
            logI "Need to build" >> newBuild logger conf directives.language
    resolveResult conf

  logI $ "Build complete, executing script: " <> result
  pure $ result </> "bin" </> conf.scriptName
