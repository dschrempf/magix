-- |
-- Module      :  Magix.Directives
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
  ( pShebang,
    pMagixDirective,
    pLanguageDirectives,
    pDirectives,
    getDirectives,
  )
where

import Control.Exception (Exception)
import Data.Bifunctor (Bifunctor (..))
import Data.Text (Text)
import Magix.Languages.Common.Directives (Parser, pDirectiveWithValue)
import Magix.Languages.Directives (Directives, getDirectivesParser)
import Magix.Languages.Language (Language (..), getLanguageNameLowercase)
import Text.Megaparsec
  ( MonadParsec (notFollowedBy),
    choice,
    chunk,
    errorBundlePretty,
    parse,
  )
import Text.Megaparsec.Char (hspace, newline, space, string)
import Prelude hiding (readFile)

pShebang :: Parser Text
pShebang = pDirectiveWithValue "/usr/bin/env" (chunk "magix")

pLanguage :: Parser Language
pLanguage = choice $ map pAnyLanguage [minBound .. maxBound :: Language]
  where
    pAnyLanguage language = language <$ string (getLanguageNameLowercase language)

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
