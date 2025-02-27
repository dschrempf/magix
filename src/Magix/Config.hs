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
  ( Config (..),
    getConfig,
  )
where

import Control.Exception (throwIO)
import Data.ByteString (ByteString)
import Magix.Hash (getMagixHash)
import Magix.NixpkgsPath (getDefaultNixpkgsPath)
import Magix.Options (Options (..))
import Magix.Paths (getBuildDir, getBuildExprPath, getLockPath, getResultLinkPath, getScriptLinkPath)
import System.Directory (canonicalizePath)
import System.Environment.XDG.BaseDir (getUserCacheDir)
import System.FilePath (takeBaseName)
import Prelude hiding (readFile)

data Config = Config
  { scriptPath :: !FilePath,
    scriptName :: !String,
    nixpkgsPath :: !FilePath,
    -- | See `getMagixHash`.
    magixHash :: !ByteString,
    -- | Cache directory containing Nix expressions and build results.
    cacheDir :: !FilePath,
    -- | File used for locking the build.
    lockPath :: !FilePath,
    -- | Sanitized path of the link to the original script.
    scriptLinkPath :: !FilePath,
    -- | Directory containing the Nix expression building the result.
    buildDir :: !FilePath,
    -- | The Nix expression building the result.
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

getConfig :: Options -> ByteString -> IO Config
getConfig opts scriptContents = do
  scriptPath <- canonicalizePath p
  cacheDir <- maybe (getUserCacheDir "magix") canonicalizePath opts.cachePath
  nixpkgsPath <- maybe getDefaultNixpkgsPathOrFail canonicalizePath opts.nixpkgsPath
  let scriptName = takeBaseName p
      magixHash = getMagixHash nixpkgsPath scriptContents
      lockPath = getLockPath cacheDir magixHash scriptName
      scriptLinkPath = getScriptLinkPath cacheDir magixHash scriptName
      buildDir = getBuildDir cacheDir magixHash scriptName
      buildExprPath = getBuildExprPath buildDir
      resultLinkPath = getResultLinkPath cacheDir magixHash scriptName
  pure $ Config {..}
  where
    p = opts.scriptPath
