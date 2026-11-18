-- |
-- Module      :  Magix.Language.Directives
-- Description :  Pooled language directives
-- Copyright   :  2025 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Apr 22 14:10:46 2025.
--
-- Bijective map between `Language` and `LanguageDirectives`.
module Magix.Language.Directives
  ( LanguageDirectives (..),
    DirectiveParser (..),
    getDirectiveParser,
    getLanguage,
  )
where

import Magix.Language.Bash.Directives (BashDirectives, pBashDirective)
import Magix.Language.Common.Directives (Parser)
import Magix.Language.Haskell.Directives (HaskellDirectives, pHaskellDirective)
import Magix.Language.Language (Language (..))
import Magix.Language.Python.Directives (PythonDirectives, pPythonDirective)

data LanguageDirectives
  = BashD !BashDirectives
  | HaskellD !HaskellDirectives
  | PythonD !PythonDirectives
  deriving (Eq, Show)

data DirectiveParser
  = forall d.
    (Monoid d) =>
    DirectiveParser
      -- | One directive line of a language.
      !(Parser d)
      -- | The language directives of all lines combined.
      !(d -> LanguageDirectives)

getDirectiveParser :: Language -> DirectiveParser
getDirectiveParser l = case l of
  Bash -> DirectiveParser pBashDirective BashD
  Haskell -> DirectiveParser pHaskellDirective HaskellD
  Python -> DirectiveParser pPythonDirective PythonD

getLanguage :: LanguageDirectives -> Language
getLanguage (BashD _) = Bash
getLanguage (HaskellD _) = Haskell
getLanguage (PythonD _) = Python
