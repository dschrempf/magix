-- |
-- Module      :  Magix.Language.Common.Expression
-- Description :  Common definitions related to handling Nix expressions
-- Copyright   :  2025 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Apr 11 06:36:34 2025.
module Magix.Language.Common.Expression
  ( Replacement,
    getCommonReplacements,
  )
where

import Data.Text (Text, pack)

type Replacement = (Text, Text)

-- | The replacements of the script name, and of its source: the file @script@
-- next to the expression.
getCommonReplacements :: String -> [Replacement]
getCommonReplacements scriptName =
  [ ("__SCRIPT_NAME__", pack scriptName),
    ("__SCRIPT_SOURCE__", "./script")
  ]
