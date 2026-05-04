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
import Data.Text (unpack)
import Data.Version (Version (versionBranch))
import GHC.Generics (Generic)
import Magix.BuildMode (BuildMode (..))
import Paths_magix (version)
import Prelude hiding (init)

data MagixHashContents = MagixHashContents
  { buildMode :: !BuildMode,
    scriptPath :: !FilePath,
    scriptContents :: !ByteString
  }
  deriving (Eq, Show, Generic)

toByteStringWith :: (a -> Builder) -> [a] -> ByteString
toByteStringWith f = toStrict . toLazyByteString . mconcat . map f

buildModeBytes :: BuildMode -> ByteString
buildModeBytes (ChannelBuild path) = toByteStringWith charUtf8 $ "channel:" <> path
buildModeBytes (FlakeBuild ref) = toByteStringWith charUtf8 $ "flake:" <> unpack ref

getMagixHash :: MagixHashContents -> ByteString
getMagixHash x =
  finalize $
    Foldable.foldl'
      update
      init
      [ toByteStringWith intDec $ versionBranch version,
        buildModeBytes x.buildMode,
        toByteStringWith charUtf8 x.scriptPath,
        x.scriptContents
      ]
