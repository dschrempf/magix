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
    resolveResult,
    nixBuildEnvironment,
  )
where

import Control.Exception (finally)
import Control.Monad (unless)
import Data.List (isPrefixOf)
import Data.Text (Text)
import Data.Text.IO (writeFile)
import Magix.BuildMode (Nixpkgs (..))
import Magix.Cache (recordInvokedPath, removeSuperseded)
import Magix.Config (Config (..))
import Magix.Expression (getFlakeWrapper)
import Magix.LockedFlake (LockedFlake (..))
import Magix.Paths (getBuildExprPath, getPendingBuildDir)
import System.Directory
  ( canonicalizePath,
    copyFile,
    createDirectoryIfMissing,
    doesDirectoryExist,
    removePathForcibly,
    renameDirectory,
  )
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..), exitFailure)
import System.FileLock (SharedExclusive (..), withFileLock)
import System.FilePath ((</>))
import System.Log.Logger (Logger, Priority (..), logL)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Prelude hiding (writeFile)

data BuildStatus = HasBeenBuilt | NeedToBuild deriving (Eq, Show)

getBuildStatus :: Config -> IO BuildStatus
getBuildStatus c = do
  resultDirExists <- doesDirectoryExist c.resultLinkPath
  pure $ if resultDirExists then HasBeenBuilt else NeedToBuild

build :: Logger -> Config -> Text -> IO ()
build logger cfg expr = do
  let logD = logL logger DEBUG
      logE = logL logger ERROR
      pendingDir = getPendingBuildDir cfg.buildDir
  logD $ "Recording invoked path: " <> cfg.invokedPathLink
  recordInvokedPath cfg.slotDir cfg.invokedPath
  -- Nix replaces the out-link only on success, so a failed build, also a forced
  -- one, keeps the previous build. The out-link is the result link itself: Nix
  -- roots the result by the path of the out-link.
  flip finally (removePathForcibly pendingDir) $ do
    logD $ "Creating the pending build directory: " <> pendingDir
    removePathForcibly pendingDir
    createDirectoryIfMissing True pendingDir
    logD $ "Copying script to build directory: " <> pendingDir </> "script"
    copyFile cfg.scriptPath (pendingDir </> "script")
    let exprPath = getBuildExprPath pendingDir
    logD $ "Writing Magix Nix expression: " <> exprPath
    writeFile exprPath expr
    (exitCode, stdOut, stdErr) <- case cfg.nixpkgs of
      NixpkgsPath nixpkgsPath -> do
        logD "Building with 'nix-build'"
        nixBuildEnv <- nixBuildEnvironment <$> getEnvironment
        -- The templates import <nixpkgs>; '-I' makes it the hashed Nixpkgs, which
        -- `NIX_PATH` may name differently or not at all.
        readCreateProcessWithExitCode
          ( proc
              "nix-build"
              [ "-I",
                "nixpkgs=" <> nixpkgsPath,
                "--out-link",
                cfg.resultLinkPath,
                pendingDir
              ]
          )
            { env = Just nixBuildEnv
            }
          ""
      NixpkgsFlake flake -> do
        logD "Writing universal Flake wrapper"
        wrapper <- getFlakeWrapper flake.ref
        let flakePath = pendingDir </> "flake.nix"
        writeFile flakePath wrapper
        logD "Building with 'nix build'"
        readProcessWithExitCode
          "nix"
          [ "build",
            "--no-write-lock-file",
            "--out-link",
            cfg.resultLinkPath,
            pendingDir
          ]
          ""
    let success = exitCode == ExitSuccess
        logFun = if success then logD else logE
    unless (null stdOut) $ logFun $ "Build output ('stdout'):\n" <> stdOut
    -- I would prefer to always log this with level "WARNING", but `nix-build`
    -- does use `stderr` quite extensively.
    unless (null stdErr) $ logFun $ "Build output ('stderr'):\n" <> stdErr
    unless success $ do
      logE $ "Build failed with code: " <> show exitCode
      exitFailure
    logD $ "Replacing the build directory: " <> cfg.buildDir
    removePathForcibly cfg.buildDir
    renameDirectory pendingDir cfg.buildDir
  removeSuperseded logger cfg

-- | The environment of @nix-build@ without the variables Nixpkgs reads while it
-- evaluates, such as @NIXPKGS_ALLOW_UNFREE@: the cache hash misses them. A
-- flake build evaluates purely and reads none.
nixBuildEnvironment :: [(String, String)] -> [(String, String)]
nixBuildEnvironment = filter (not . isPrefixOf "NIXPKGS_" . fst)

-- | Run the action holding the lock of the slot, and the cache lock shared:
-- builds of different slots run in parallel, and cleaning the cache waits for
-- them.
withBuildLock :: Logger -> Config -> IO a -> IO a
withBuildLock logger cfg action = do
  let logD = logL logger DEBUG
  createDirectoryIfMissing True cfg.cacheDir
  logD "Acquiring locks"
  x <- withFileLock cfg.cacheLockPath Shared $ \_ -> do
    createDirectoryIfMissing True cfg.slotDir
    withFileLock cfg.lockPath Exclusive $ const action
  logD "Released locks"
  pure x

-- | The store path of the build result. Call it holding the lock: a rebuild of
-- a changed script removes the link.
resolveResult :: Config -> IO FilePath
resolveResult cfg = canonicalizePath cfg.resultLinkPath
