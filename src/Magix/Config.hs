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
import Magix.Hash (MagixHashContents (..), getMagixHash)
import Magix.NixpkgsPath (getDefaultNixpkgsPath)
import Magix.Options (Options (..))
import Magix.Paths (getBuildDir, getLockPath, getResultLinkPath)
import System.Directory (canonicalizePath)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath (takeBaseName, (</>))
import Prelude hiding (readFile)

data Config = Config
  { scriptPath :: !FilePath,
    scriptName :: !String,
    buildMode :: !BuildMode,
    -- | See `getMagixHash`.
    magixHash :: !ByteString,
    -- | Cache directory containing Nix expressions and build results.
    cacheDir :: !FilePath,
    -- | File used for locking the build.
    lockPath :: !FilePath,
    -- | Directory containing the Nix expression building the result.
    buildDir :: !FilePath,
    -- | The Nix expression building the result (always default.nix).
    buildExprPath :: !FilePath,
    -- | Link to directory containing the result of the build.
    resultLinkPath :: !FilePath
  }
  deriving (Eq, Show)

getDefaultNixpkgsPathOrFail :: IO FilePath
getDefaultNixpkgsPathOrFail = do
  mr <- getDefaultNixpkgsPath
  case mr of
    Left err -> do
      putStrLn "Could not retrieve Nixpkgs path from NIX_PATH"
      throwIO err
    Right np -> pure np

-- | Resolve the build mode.
-- Priority: script's #!nixpkgs directive > CLI/env > NIX_PATH channel
resolveBuildMode :: Options -> Maybe Text -> IO BuildMode
resolveBuildMode opts mDirectiveRef =
  case (FlakeBuild <$> mDirectiveRef) <|> opts.buildMode of
    Just (ChannelBuild p) -> ChannelBuild <$> canonicalizePath p
    Just (FlakeBuild r) -> pure $ FlakeBuild r
    Nothing -> ChannelBuild <$> getDefaultNixpkgsPathOrFail

getConfig :: Options -> ByteString -> Maybe Text -> IO Config
getConfig opts scriptContents mDirectiveRef = do
  scriptPath <- canonicalizePath p
  cacheDir <- getCacheDir opts.cachePath
  buildMode <- resolveBuildMode opts mDirectiveRef
  let scriptName = takeBaseName p
      magixHash =
        getMagixHash $
          MagixHashContents
            { buildMode = buildMode,
              scriptPath = scriptPath,
              scriptContents = scriptContents
            }
      lockPath = getLockPath cacheDir magixHash scriptName
      buildDir = getBuildDir cacheDir magixHash scriptName
      buildExprPath = buildDir </> "default.nix"
      resultLinkPath = getResultLinkPath cacheDir magixHash scriptName
  pure $ Config {..}
  where
    p = opts.scriptPath

getCacheDir :: Maybe FilePath -> IO FilePath
getCacheDir mCachePath = maybe (getUserCacheDir "magix") canonicalizePath mCachePath
