-- |
-- Module      :  Magix.Languages.Directives
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
-- Bijective map between `Language` and `Directives`.
module Magix.Languages.Directives
  ( Directives (..),
    getDirectivesParser,
    getLanguage,
  )
where

import Control.Applicative (Alternative (..))
import Magix.Languages.Bash.Directives (BashDirectives, pBashDirectives)
import Magix.Languages.Common.Directives (Parser)
import Magix.Languages.Haskell.Directives (HaskellDirectives, pHaskellDirectives)
import Magix.Languages.Language (Language (..))
import Magix.Languages.Python.Directives (PythonDirectives, pPythonDirectives)
import Text.Megaparsec (try)
import Text.Megaparsec.Char (newline)

data Directives
  = BashD !BashDirectives
  | HaskellD !HaskellDirectives
  | PythonD !PythonDirectives
  deriving (Eq, Show)

getDirectivesParser :: Language -> Parser Directives
getDirectivesParser l = case l of
  Bash -> BashD <$> withNewline pBashDirectives
  Haskell -> HaskellD <$> withNewline pHaskellDirectives
  Python -> PythonD <$> withNewline pPythonDirectives
  where
    withNewline p = try (newline *> p) <|> mempty

getLanguage :: Directives -> Language
getLanguage (BashD _) = Bash
getLanguage (HaskellD _) = Haskell
getLanguage (PythonD _) = Python
