-- |
-- Module      :  Magix.Languages.Haskell.Expression
-- Description :  Build Haskell command lines
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 13:36:32 2024.
module Magix.Languages.Haskell.Expression
  ( getHaskellReplacements,
  )
where

import Data.Text (unwords)
import Magix.Languages.Expression (Replacement)
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Prelude hiding (readFile, unwords)

getHaskellReplacements :: HaskellDirectives -> [Replacement]
getHaskellReplacements (HaskellDirectives ps fs) =
  [ ("__HASKELL_PACKAGES__", unwords ps),
    ("__GHC_FLAGS__", unwords fs)
  ]
