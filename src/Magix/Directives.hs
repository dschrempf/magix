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
  ( Directives (..),
    getLanguageName,
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
import Data.Text (Text, pack, unpack)
import Magix.Languages.Bash.Directives (BashDirectives, pBashDirectives)
import Magix.Languages.Common.Directives (Parser, pDirectiveWithValue)
import Magix.Languages.Haskell.Directives (HaskellDirectives, pHaskellDirectives)
import Magix.Languages.Python.Directives (PythonDirectives, pPythonDirectives)
import Text.Megaparsec (MonadParsec (notFollowedBy, try), chunk, errorBundlePretty, parse)
import Text.Megaparsec.Char (alphaNumChar, hspace, newline, space)
import Prelude hiding (readFile)

data Directives
  = Bash !BashDirectives
  | Haskell !HaskellDirectives
  | Python !PythonDirectives
  deriving (Eq, Show)

-- | Use the language name to find the Nix expression template.
getLanguageName :: Directives -> String
getLanguageName (Bash _) = "Bash"
getLanguageName (Haskell _) = "Haskell"
getLanguageName (Python _) = "Python"

pShebang :: Parser Text
pShebang = pDirectiveWithValue "/usr/bin/env" (chunk "magix")

pLanguage :: Parser Text
pLanguage = pack <$> some alphaNumChar

pMagixDirective :: Parser Text
pMagixDirective = pDirectiveWithValue "magix" pLanguage <* hspace

pLanguageDirectives :: Parser Directives
pLanguageDirectives = do
  language <- pMagixDirective
  let withNewline p = try (newline *> p) <|> mempty
  directives <- case language of
    "bash" -> Bash <$> withNewline pBashDirectives
    "haskell" -> Haskell <$> withNewline pHaskellDirectives
    "python" -> Python <$> withNewline pPythonDirectives
    unknownLanguage -> fail $ "unknown language: " <> unpack unknownLanguage
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
