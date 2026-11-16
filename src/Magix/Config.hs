{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      :  Magix.Config
-- Description :  Magix configuration
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:12:29 2024.
module Magix.Config
  ( BuildMode (..),
    Config (..),
    getConfig,
    getCacheDir,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Magix.BuildMode (BuildMode (..))
import Magix.Env (NixEnv (..), nixEnvDesc)
import Magix.Hash (MagixHashContents (..), getMagixHash, getSlotHash)
import Magix.NixpkgsPath (getDefaultNixpkgsPath, resolveNixpkgs)
import Magix.Options (Options (..))
import Magix.Paths
  ( getBuildDir,
    getBuildExprPath,
    getCacheLockPath,
    getInvokedPathLink,
    getLockPath,
    getResultLinkPath,
    getSlotDir,
  )
import System.Directory (canonicalizePath, makeAbsolute)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.Exit (die)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile)

data Config = Config
  { -- | Absolute path the script is invoked by; see `getSlotHash`.
    invokedPath :: !FilePath,
    -- | Canonical path of the script, which the build copies.
    scriptPath :: !FilePath,
    scriptName :: !String,
    buildMode :: !BuildMode,
    -- | See `getMagixHash`.
    magixHash :: !ByteString,
    -- | Cache directory containing one slot per invoked path.
    cacheDir :: !FilePath,
    -- | See `getCacheLockPath`.
    cacheLockPath :: !FilePath,
    -- | Directory holding the build of the script; a rebuild replaces it.
    slotDir :: !FilePath,
    -- | File used for locking the slot.
    lockPath :: !FilePath,
    -- | Symbolic link to the invoked path, for removing stale slots.
    invokedPathLink :: !FilePath,
    -- | Directory containing the Nix expression building the result.
    buildDir :: !FilePath,
    -- | The Nix expression building the result (always default.nix).
    buildExprPath :: !FilePath,
    -- | Link to directory containing the result of the build.
    resultLinkPath :: !FilePath
  }
  deriving (Eq, Show)

getDefaultNixpkgsPathOrFail :: Maybe String -> IO FilePath
getDefaultNixpkgsPathOrFail Nothing = die $ fst nixEnvDesc.nixPath <> " is not set"
getDefaultNixpkgsPathOrFail (Just np) = case getDefaultNixpkgsPath np of
  Left err -> do
    putStrLn $ "Could not retrieve Nixpkgs path from " <> fst nixEnvDesc.nixPath
    throwIO err
  Right entries -> resolveFirst entries
  where
    -- Return the first entry resolving to a Nixpkgs path.
    resolveFirst [] =
      die $ "Could not find Nixpkgs in " <> fst nixEnvDesc.nixPath <> ": " <> np
    resolveFirst (e : es) = resolveNixpkgs e >>= maybe (resolveFirst es) pure

-- | Resolve the build mode.
-- Priority: script's #!nixpkgs directive > CLI/env > NIX_PATH channel
resolveBuildMode :: Options -> Maybe Text -> IO BuildMode
resolveBuildMode opts mDirectiveRef =
  case (FlakeBuild <$> mDirectiveRef) <|> opts.buildMode of
    Just (ChannelBuild p) -> ChannelBuild <$> canonicalizePath p
    Just (FlakeBuild r) -> pure $ FlakeBuild r
    Nothing -> ChannelBuild <$> getDefaultNixpkgsPathOrFail opts.nixEnv.nixPath

getConfig :: Options -> ByteString -> Maybe Text -> IO Config
getConfig opts scriptContents mDirectiveRef = do
  invokedPath <- makeAbsolute p
  scriptPath <- canonicalizePath p
  cacheDir <- getCacheDir opts.cachePath
  buildMode <- resolveBuildMode opts mDirectiveRef
  let scriptName = takeBaseName p
      magixHash =
        getMagixHash $
          MagixHashContents
            { buildMode = buildMode,
              scriptContents = scriptContents
            }
      cacheLockPath = getCacheLockPath cacheDir
      slotDir = getSlotDir cacheDir (getSlotHash invokedPath) scriptName
      lockPath = getLockPath slotDir
      invokedPathLink = getInvokedPathLink slotDir
      buildDir = getBuildDir slotDir magixHash
      buildExprPath = getBuildExprPath buildDir
      resultLinkPath = getResultLinkPath slotDir magixHash
  pure $ Config {..}
  where
    p = opts.scriptPath

getCacheDir :: Maybe FilePath -> IO FilePath
getCacheDir mCachePath = maybe (getUserCacheDir "magix") canonicalizePath mCachePath
