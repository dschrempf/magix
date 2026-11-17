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
    cleanCache,
  )
where

import Control.Exception (displayException, throwIO, try)
import Data.ByteString (readFile)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8')
import GHC.IO.Exception (IOException (..))
import Magix.Build (BuildStatus (..), build, getBuildStatus, resolveResult, withBuildLock)
import Magix.Cache (removeCacheContents, removeStaleEntries)
import Magix.Config (Config (..), getCacheDir, getConfig)
import Magix.Directives (Directives (..), getDirectives)
import Magix.Expression (getNixExpression)
import Magix.Language.Directives (LanguageDirectives)
import Magix.Options (CleanMode (..), CleanOptions (..), Options (..), Rebuild (..))
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Log.Logger (Logger, Priority (..), logL)
import Prelude hiding (readFile)

newBuild :: Logger -> Config -> LanguageDirectives -> IO ()
newBuild logger conf dirs = do
  let logD = logL logger DEBUG
  logD "Getting Magix Nix expression"
  expr <- getNixExpression conf dirs
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

-- | Build the script, or reuse its build, and return its executable.
--
-- Log the reason and exit with failure if the script cannot be read; throw
-- if it cannot be decoded, or its directives cannot be parsed.
runMagix :: Logger -> Options -> IO FilePath
runMagix logger opts = do
  let logD = logL logger DEBUG
      logI = logL logger INFO
      logE = logL logger ERROR
  logD $ "Options are: " <> show opts

  let path = opts.scriptPath
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

  conf <- getConfig opts scriptBytes directives.nixpkgsRef
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
