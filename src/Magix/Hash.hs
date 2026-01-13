-- |
-- Module      :  Magix.Hash
-- Description :  Create hashes of Magix configurations
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 31 06:47:58 2024.
module Magix.Hash
  ( MagixHashContents (..),
    getMagixHash,
  )
where

import Crypto.Hash.SHA256 (finalize, init, update)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (Builder, charUtf8, intDec, toLazyByteString)
import Data.Foldable qualified as Foldable
import Data.Version (Version (versionBranch))
import GHC.Generics (Generic)
import Paths_magix (version)
import Prelude hiding (init)

data MagixHashContents = MagixHashContents
  { nixpkgsPath :: !FilePath,
    scriptPath :: !FilePath,
    scriptContents :: !ByteString
  }
  deriving (Eq, Show, Generic)

toByteStringWith :: (a -> Builder) -> [a] -> ByteString
toByteStringWith f = toStrict . toLazyByteString . mconcat . map f

getMagixHash :: MagixHashContents -> ByteString
getMagixHash x =
  finalize $
    Foldable.foldl'
      update
      init
      [ toByteStringWith intDec $ versionBranch version,
        toByteStringWith charUtf8 x.nixpkgsPath,
        toByteStringWith charUtf8 x.scriptPath,
        x.scriptContents
      ]
