-- |
-- Module      :  Magix.Languages.Expression
-- Description :  Pooled language expressions
-- Copyright   :  2025 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Apr 22 14:10:46 2025.
module Magix.Languages.Expression (getLanguageReplacements) where

import Magix.Languages.Bash.Expression (getBashReplacements)
import Magix.Languages.Common.Expression (Replacement)
import Magix.Languages.Directives (Directives (BashD, HaskellD, PythonD))
import Magix.Languages.Haskell.Expression (getHaskellReplacements)
import Magix.Languages.Python.Expression (getPythonReplacements)

getLanguageReplacements :: Directives -> [Replacement]
getLanguageReplacements (BashD ds) = getBashReplacements ds
getLanguageReplacements (HaskellD ds) = getHaskellReplacements ds
getLanguageReplacements (PythonD ds) = getPythonReplacements ds
