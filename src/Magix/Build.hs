-- |
-- Module      :  Magix.Build
-- Description :  Build Magix expressions
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  not portable
--
-- Creation date: Sun Oct 20 09:48:50 2024.
module Magix.Build
  ( BuildStatus (..),
    getBuildStatus,
    build,
    withBuildLock,
  )
where

import Control.Monad (unless, when)
import Data.Text (Text)
import Data.Text.IO (writeFile)
import Magix.Config (Config (..))
import System.Directory
  ( createDirectoryIfMissing,
    doesDirectoryExist,
    doesFileExist,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Exit (ExitCode (..), exitFailure)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.Log.Logger (Logger, Priority (..), logL)
import System.Posix (createSymbolicLink)
import System.Process (readProcessWithExitCode)
import Prelude hiding (writeFile)

data BuildStatus = HasBeenBuilt | NeedToBuild deriving (Eq, Show)

getBuildStatus :: Config -> IO BuildStatus
getBuildStatus c = do
  resultDirExists <- doesDirectoryExist c.resultLinkPath
  pure $ if resultDirExists then HasBeenBuilt else NeedToBuild

removeBuild :: Logger -> Config -> IO ()
removeBuild logger cfg = do
  let logD = logL logger DEBUG
  -- Link to script.
  scriptLinkPathExists <- doesFileExist cfg.scriptLinkPath
  when scriptLinkPathExists $ do
    logD $ "Removing link to script: " <> cfg.scriptLinkPath
    removeFile cfg.scriptLinkPath
  -- Build directory.
  buildDirExists <- doesDirectoryExist cfg.buildDir
  when buildDirExists $ do
    logD $ "Removing build directory: " <> cfg.buildDir
    removeDirectoryRecursive cfg.buildDir
  -- Result directory.
  resultDirExists <- doesDirectoryExist cfg.resultLinkPath
  when resultDirExists $ do
    logD $ "Removing link to build result: " <> cfg.resultLinkPath
    removeFile cfg.resultLinkPath

build :: Logger -> Config -> Text -> IO ()
build logger cfg expr = do
  let logD = logL logger DEBUG
      logE = logL logger ERROR
  logD "Removing previous builds"
  removeBuild logger cfg
  logD $ "Creating sanitized link to script: " <> cfg.scriptLinkPath
  createSymbolicLink cfg.scriptPath cfg.scriptLinkPath
  logD $ "Recreating the build directory: " <> cfg.buildDir
  createDirectoryIfMissing True cfg.buildDir
  -- Expression.
  let exprPath = buildExprPath cfg
  logD $ "Writing Magix Nix expression: " <> exprPath
  writeFile exprPath expr
  -- Build.
  logD "Building with nix-build"
  (exitCode, stdOut, stdErr) <-
    readProcessWithExitCode
      "nix-build"
      ["--out-link", cfg.resultLinkPath, cfg.buildDir]
      ""
  let success = exitCode == ExitSuccess
      logFun = if success then logD else logE
  unless (null stdOut) $ logFun $ "Build output ('stdout'):\n" <> stdOut
  -- I would prefer to always log this with level "WARNING", but `nix-build`
  -- does use `stderr` quite extensively.
  unless (null stdErr) $ logFun $ "Build output ('stderr'):\n" <> stdErr
  unless (exitCode == ExitSuccess) $ do
    logE $ "Build failed with code: " <> show exitCode
    exitFailure

withBuildLock :: Logger -> Config -> IO () -> IO ()
withBuildLock logger cfg action = do
  -- Make sure the cache directory exists.
  createDirectoryIfMissing True cfg.cacheDir
  let logD = logL logger DEBUG
  logD "Acquiring lock"
  withFileLock cfg.lockPath Exclusive $ const action
  logD "Released lock"
