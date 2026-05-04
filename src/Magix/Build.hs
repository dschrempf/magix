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
import Magix.BuildMode (BuildMode (..))
import Magix.Config (Config (..))
import Magix.Expression (getFlakeWrapper)
import System.Directory
  ( copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Exit (ExitCode (..), exitFailure)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath ((</>))
import System.Log.Logger (Logger, Priority (..), logL)
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
  logD $ "Recreating the build directory: " <> cfg.buildDir
  createDirectoryIfMissing True cfg.buildDir
  logD $ "Copying script to build directory: " <> cfg.buildDir </> "script"
  copyFile cfg.scriptPath (cfg.buildDir </> "script")
  logD $ "Writing Magix Nix expression: " <> cfg.buildExprPath
  writeFile cfg.buildExprPath expr
  (exitCode, stdOut, stdErr) <- case cfg.buildMode of
    ChannelBuild _ -> do
      logD "Building with 'nix-build'"
      readProcessWithExitCode
        "nix-build"
        [ "--out-link",
          cfg.resultLinkPath,
          cfg.buildDir
        ]
        ""
    FlakeBuild ref -> do
      logD "Writing universal Flake wrapper"
      wrapper <- getFlakeWrapper ref
      let flakePath = cfg.buildDir </> "flake.nix"
      writeFile flakePath wrapper
      logD "Building with 'nix build'"
      readProcessWithExitCode
        "nix"
        [ "build",
          "--no-write-lock-file",
          "--out-link",
          cfg.resultLinkPath,
          cfg.buildDir
        ]
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
