-- |
-- Module      :  Magix.Languages.Expression
-- Description :  Common definitions related to handling Nix expressions
-- Copyright   :  2025 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Apr 11 06:36:34 2025.
module Magix.Languages.Common.Expression
  ( Replacement,
    getCommonReplacements,
  )
where

import Data.Text (Text, pack)
import Magix.Config (Config (..))

type Replacement = (Text, Text)

getCommonReplacements :: Config -> [Replacement]
getCommonReplacements c =
  [ ("__SCRIPT_NAME__", pack $ scriptName c),
    ("__SCRIPT_SOURCE__", pack $ scriptLinkPath c)
  ]
