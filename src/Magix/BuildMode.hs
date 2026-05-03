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
  = -- | Use a Nixpkgs channel; the store path is included in the cache hash.
    ChannelBuild !FilePath
  | -- | Use a pinned Nixpkgs flake reference; the reference is included in the cache hash.
    FlakeBuild !Text
  deriving (Eq, Show, Generic)
