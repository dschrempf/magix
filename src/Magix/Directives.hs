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
    pDirectives,
    getDirectives,
  )
where

import Control.Applicative (Alternative (..))
import Control.Exception (Exception)
import Data.Bifunctor (Bifunctor (..))
import Data.Text (Text, unpack)
import Magix.Directives.Common (Parser, pDirectiveWithValue, pMagixDirective)
import Magix.Languages.Bash.Directives (BashDirectives, pBashDirectives)
import Magix.Languages.Haskell.Directives (HaskellDirectives, pHaskellDirectives)
import Magix.Languages.Python.Directives (PythonDirectives, pPythonDirectives)
import Text.Megaparsec (MonadParsec (notFollowedBy, try), chunk, errorBundlePretty, parse)
import Text.Megaparsec.Char (hspace, newline, space)
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

pLanguageSpecificDirectives :: Parser Directives
pLanguageSpecificDirectives = do
  language <- pMagixDirective
  let newlineWith parser = try (newline *> parser) <|> mempty
  directives <- case language of
    "bash" -> Bash <$> newlineWith pBashDirectives
    "haskell" -> Haskell <$> newlineWith pHaskellDirectives
    "python" -> Python <$> newlineWith pPythonDirectives
    unknownLanguage -> fail $ "unknown language: " <> unpack unknownLanguage
  notFollowedBy $ space *> chunk "#!"
  pure directives

pDirectives :: Parser Directives
pDirectives = pShebang *> hspace *> newline *> space *> pLanguageSpecificDirectives

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
