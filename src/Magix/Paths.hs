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
  ( getBuildDir,
    getLockPath,
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

getCommonPrefix :: FilePath -> ByteString -> String -> FilePath
getCommonPrefix cacheDir hash name =
  cacheDir
    </> showHash hash
      <> "-"
      <> map replaceProblematicChars name

getLockPath :: FilePath -> ByteString -> String -> FilePath
getLockPath cacheDir hash name = getCommonPrefix cacheDir hash name <> ".lock"

getBuildDir :: FilePath -> ByteString -> String -> FilePath
getBuildDir cacheDir hash name = getCommonPrefix cacheDir hash name <> "-build"

getBuildExprPath :: FilePath -> FilePath
getBuildExprPath buildDir = buildDir </> "default.nix"

getResultLinkPath :: FilePath -> ByteString -> String -> FilePath
getResultLinkPath cacheDir hash name = getCommonPrefix cacheDir hash name <> "-result"
