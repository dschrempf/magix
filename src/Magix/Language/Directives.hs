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
    getDirectivesParser,
    getLanguage,
  )
where

import Magix.Language.Bash.Directives (BashDirectives, pBashDirectives)
import Magix.Language.Common.Directives (Parser)
import Magix.Language.Haskell.Directives (HaskellDirectives, pHaskellDirectives)
import Magix.Language.Language (Language (..))
import Magix.Language.Python.Directives (PythonDirectives, pPythonDirectives)

data LanguageDirectives
  = BashD !BashDirectives
  | HaskellD !HaskellDirectives
  | PythonD !PythonDirectives
  deriving (Eq, Show)

getDirectivesParser :: Language -> Parser LanguageDirectives
getDirectivesParser l = case l of
  Bash -> BashD <$> pBashDirectives
  Haskell -> HaskellD <$> pHaskellDirectives
  Python -> PythonD <$> pPythonDirectives

getLanguage :: LanguageDirectives -> Language
getLanguage (BashD _) = Bash
getLanguage (HaskellD _) = Haskell
getLanguage (PythonD _) = Python
