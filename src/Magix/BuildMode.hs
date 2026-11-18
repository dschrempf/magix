-- |
-- Module      :  Magix.BuildMode
-- Description :  Build mode selection
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun May  3 20:27:22 2026.
module Magix.BuildMode
  ( BuildMode (..),
    Nixpkgs (..),
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)
import Magix.LockedFlake (LockedFlake)

-- | The Nixpkgs asked for.
data BuildMode
  = -- | A path, given by @--nixpkgs-path@.
    ChannelBuild !FilePath
  | -- | A flake reference.
    FlakeBuild !Text
  deriving (Eq, Show, Generic)

-- | The Nixpkgs a build uses; the cache hash includes it.
data Nixpkgs
  = -- | A canonical path, given by @--nixpkgs-path@ or found in @NIX_PATH@ (a
    -- @flake:@ entry resolved through the flake registries). A channel symlink
    -- resolves to its store path, so a channel update rebuilds; a directory
    -- outside the store keeps its builds when its contents change.
    NixpkgsPath !FilePath
  | -- | Built with @nix build@.
    NixpkgsFlake !LockedFlake
  deriving (Eq, Show, Generic)
