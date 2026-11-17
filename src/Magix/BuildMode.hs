-- |
-- Module      :  Magix.BuidMode
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
  )
where

import Data.Text (Text)
import GHC.Generics (Generic)

data BuildMode
  = -- | Use the Nixpkgs at a path, given by @--nixpkgs-path@ (canonicalized) or
    -- found in @NIX_PATH@ (a @flake:@ entry resolved through the flake
    -- registries); the path is included in the cache hash. A path that is no
    -- store path, such as a channel symlink, keeps its builds when its contents
    -- change.
    ChannelBuild !FilePath
  | -- | Use a pinned Nixpkgs Flake reference; the reference is included in the cache hash.
    FlakeBuild !Text
  deriving (Eq, Show, Generic)
