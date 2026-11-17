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
    resolveBuildMode,
    getConfig,
    getCacheDir,
    getScriptName,
  )
where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Magix.BuildMode (BuildMode (..))
import Magix.Env (NixEnv (..), nixEnvDesc)
import Magix.FlakeRegistry (resolveFlakeRef)
import Magix.Hash (MagixHashContents (..), getMagixHash, getSlotHash)
import Magix.NixpkgsPath (canonicalIfExists, getDefaultNixpkgsPath, renderEntryError, resolveNixpkgs)
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

getDefaultNixpkgsPathOrFail :: NixEnv (Maybe String) -> IO FilePath
getDefaultNixpkgsPathOrFail env = case env.nixPath of
  Nothing -> die $ fst nixEnvDesc.nixPath <> " is not set"
  Just np -> resolveFirst np $ getDefaultNixpkgsPath np
  where
    -- Return the first entry resolving to a Nixpkgs path.
    resolveFirst np [] =
      die $ "Could not find Nixpkgs in " <> fst nixEnvDesc.nixPath <> ": " <> np
    resolveFirst np (e : es) =
      resolveNixpkgs (resolveFlakeRef env) e
        >>= either (die . renderEntryError) (maybe (resolveFirst np es) pure)

-- | Resolve the build mode.
-- Priority: script's #!nixpkgs directive > CLI/env > NIX_PATH channel
resolveBuildMode :: Options -> Maybe Text -> IO BuildMode
resolveBuildMode opts mDirectiveRef =
  case (FlakeBuild <$> mDirectiveRef) <|> opts.buildMode of
    Just (ChannelBuild p) ->
      canonicalIfExists p
        >>= maybe (die $ "Cannot use the Nixpkgs path '" <> p <> "' of '--nixpkgs-path': it does not exist.") (pure . ChannelBuild)
    Just (FlakeBuild r) -> pure $ FlakeBuild r
    Nothing -> ChannelBuild <$> getDefaultNixpkgsPathOrFail opts.nixEnv

getConfig :: Options -> ByteString -> Maybe Text -> IO Config
getConfig opts scriptContents mDirectiveRef = do
  invokedPath <- makeAbsolute p
  scriptPath <- canonicalizePath p
  cacheDir <- getCacheDir opts.cachePath
  buildMode <- resolveBuildMode opts mDirectiveRef
  let scriptName = getScriptName p
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

-- | The name of the script, and of its executable: the base name of its path.
getScriptName :: FilePath -> String
getScriptName = takeBaseName

getCacheDir :: Maybe FilePath -> IO FilePath
getCacheDir mCachePath = maybe (getUserCacheDir "magix") canonicalizePath mCachePath
