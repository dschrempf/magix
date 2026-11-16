-- |
-- Module      :  Magix.Paths
-- Description :  Get paths to cache files
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Oct 21 17:06:03 2024.
module Magix.Paths
  ( getCacheLockPath,
    getSlotDir,
    getLockPath,
    getInvokedPathLink,
    getBuildDir,
    getBuildExprPath,
    getResultLinkPath,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Data.Char (isAlphaNum)
import System.FilePath ((</>))

showHash :: ByteString -> String
showHash = unpack . toLazyByteString . byteStringHex

replaceProblematicChars :: Char -> Char
replaceProblematicChars c
  | isAlphaNum c = c
  | otherwise = '-'

-- | The lock builds hold shared, and cleaning the cache exclusively.
getCacheLockPath :: FilePath -> FilePath
getCacheLockPath cacheDir = cacheDir </> "lock"

-- | The directory holding the builds of one script, given the cache directory,
-- the slot hash, and the script name.
getSlotDir :: FilePath -> ByteString -> String -> FilePath
getSlotDir cacheDir slotHash name =
  cacheDir
    </> showHash slotHash
      <> "-"
      <> map replaceProblematicChars name

getLockPath :: FilePath -> FilePath
getLockPath slotDir = slotDir </> "lock"

-- | Symbolic link to the path the script is invoked by; see `recordInvokedPath`.
getInvokedPathLink :: FilePath -> FilePath
getInvokedPathLink slotDir = slotDir </> "path"

getBuildDir :: FilePath -> ByteString -> FilePath
getBuildDir slotDir hash = slotDir </> showHash hash <> "-build"

getBuildExprPath :: FilePath -> FilePath
getBuildExprPath buildDir = buildDir </> "default.nix"

getResultLinkPath :: FilePath -> ByteString -> FilePath
getResultLinkPath slotDir hash = slotDir </> showHash hash <> "-result"
