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
    getSlotHash,
  )
where

import Crypto.Hash.SHA256 (finalize, hash, init, update)
import Data.ByteString (ByteString, toStrict)
import Data.ByteString.Builder (Builder, charUtf8, intDec, toLazyByteString)
import Data.Foldable qualified as Foldable
import Data.Text (unpack)
import Data.Version (Version (versionBranch))
import GHC.Generics (Generic)
import Magix.BuildMode (BuildMode (..))
import Paths_magix (version)
import Prelude hiding (init)

-- | What a build depends on, besides the script's name, which the slot fixes
-- (see `getSlotHash`). The build reads a copy of the script, never its path.
data MagixHashContents = MagixHashContents
  { buildMode :: !BuildMode,
    scriptContents :: !ByteString
  }
  deriving (Eq, Show, Generic)

toByteStringWith :: (a -> Builder) -> [a] -> ByteString
toByteStringWith f = toStrict . toLazyByteString . mconcat . map f

buildModeBytes :: BuildMode -> ByteString
buildModeBytes (ChannelBuild path) = toByteStringWith charUtf8 $ "channel:" <> path
buildModeBytes (FlakeBuild ref) = toByteStringWith charUtf8 $ "flake:" <> unpack ref

-- | Identify a build within a slot.
getMagixHash :: MagixHashContents -> ByteString
getMagixHash x =
  finalize $
    Foldable.foldl'
      update
      init
      [ toByteStringWith intDec $ versionBranch version,
        buildModeBytes x.buildMode,
        x.scriptContents
      ]

-- | Identify a slot by the absolute path a script is invoked by, symbolic links
-- unresolved: a script installed from a flake is invoked by a stable path,
-- while its canonical path changes with every deploy.
getSlotHash :: FilePath -> ByteString
getSlotHash = hash . toByteStringWith charUtf8
