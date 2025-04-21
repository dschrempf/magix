-- |
-- Module      :  Directives
-- Description :  Parse directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Directives
  ( Language (..),
    getLanguageLowercase,
    Directives (..),
    getLanguage,
    pShebang,
    pMagixDirective,
    pLanguageDirectives,
    pDirectives,
    getDirectives,
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Data.Bifunctor (Bifunctor (..))
import Data.Text (Text, pack, toLower)
import Magix.Languages.Bash.Directives (BashDirectives, pBashDirectives)
import Magix.Languages.Common.Directives (Parser, pDirectiveWithValue)
import Magix.Languages.Haskell.Directives (HaskellDirectives, pHaskellDirectives)
import Magix.Languages.Python.Directives (PythonDirectives, pPythonDirectives)
import Text.Megaparsec (MonadParsec (notFollowedBy, try), choice, chunk, errorBundlePretty, parse)
import Text.Megaparsec.Char (hspace, newline, space, string)
import Prelude hiding (readFile)

data Language = Bash | Haskell | Python
  deriving (Eq, Show, Ord, Enum, Bounded)

getLanguageLowercase :: Language -> Text
getLanguageLowercase = toLower . pack . show

pLanguage :: Parser Language
pLanguage = choice $ map pAnyLanguage [minBound .. maxBound :: Language]
  where
    pAnyLanguage language = language <$ string (getLanguageLowercase language)

getDirectivesParser :: Language -> Parser Directives
getDirectivesParser l = case l of
  Bash -> BashD <$> withNewline pBashDirectives
  Haskell -> HaskellD <$> withNewline pHaskellDirectives
  Python -> PythonD <$> withNewline pPythonDirectives
  where
    withNewline p = try (newline *> p) <|> mempty

data Directives
  = BashD !BashDirectives
  | HaskellD !HaskellDirectives
  | PythonD !PythonDirectives
  deriving (Eq, Show)

getLanguage :: Directives -> Language
getLanguage (BashD _) = Bash
getLanguage (HaskellD _) = Haskell
getLanguage (PythonD _) = Python

pShebang :: Parser Text
pShebang = pDirectiveWithValue "/usr/bin/env" (chunk "magix")

pMagixDirective :: Parser Language
pMagixDirective = pDirectiveWithValue "magix" pLanguage <* hspace

pLanguageDirectives :: Parser Directives
pLanguageDirectives = do
  language <- pMagixDirective
  directives <- getDirectivesParser language
  notFollowedBy $ space *> chunk "#!"
  pure directives

pDirectives :: Parser Directives
pDirectives = pShebang *> hspace *> newline *> pLanguageDirectives

data DirectivesParseError = DirectivesParseError
  { _directives :: !Text,
    _err :: !String
  }
  deriving (Eq, Show)

instance Exception DirectivesParseError

getDirectives :: FilePath -> Text -> Either DirectivesParseError Directives
getDirectives p x = first fromErr $ parse pDirectives p x
  where
    fromErr e = DirectivesParseError x $ errorBundlePretty e
