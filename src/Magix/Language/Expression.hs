-- |
-- Module      :  Magix.Language.Expression
-- Description :  Pooled language expressions
-- Copyright   :  2025 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Apr 22 14:10:46 2025.
module Magix.Language.Expression (getLanguageReplacements) where

import Magix.Language.Bash.Expression (getBashReplacements)
import Magix.Language.Common.Expression (Replacement)
import Magix.Language.Directives (LanguageDirectives (..))
import Magix.Language.Haskell.Expression (getHaskellReplacements)
import Magix.Language.Python.Expression (getPythonReplacements)

getLanguageReplacements :: LanguageDirectives -> [Replacement]
getLanguageReplacements (BashD ds) = getBashReplacements ds
getLanguageReplacements (HaskellD ds) = getHaskellReplacements ds
getLanguageReplacements (PythonD ds) = getPythonReplacements ds
