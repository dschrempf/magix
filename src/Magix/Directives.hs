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
  ( Directives (..),
    pShebang,
    pMagixDirective,
    pNixpkgsDirective,
    pDirectiveLines,
    pDirectives,
    DirectivesParseError,
    getDirectives,
  )
where

import Control.Applicative ((<|>))
import Control.Exception (Exception)
import Control.Monad (void)
import Data.Bifunctor (Bifunctor (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Magix.Language.Common.Directives (Parser, pDirectiveWithValue, pLineEnd, pValue)
import Magix.Language.Directives (DirectiveParser (..), LanguageDirectives, getDirectiveParser)
import Magix.Language.Language (Language (..), getLanguageNameLowercase)
import Text.Megaparsec
  ( MonadParsec (eof, lookAhead, notFollowedBy, takeWhileP, try),
    choice,
    chunk,
    errorBundlePretty,
    getOffset,
    optional,
    parse,
    setOffset,
    skipMany,
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

pNixpkgsDirective :: Parser Text
pNixpkgsDirective = pDirectiveWithValue "nixpkgs" pValue <* hspace

data Directives = Directives
  { nixpkgsRef :: !(Maybe Text),
    language :: !LanguageDirectives
  }
  deriving (Eq, Show)

-- | One line of the directives after the shebang.
data DirectiveLine d = MagixLine !Language | NixpkgsLine !Text | LanguageLine !d

-- | The directive lines parsed so far.
data DirectiveLines d = DirectiveLines
  { magix :: !(Maybe Language),
    nixpkgs :: !(Maybe Text),
    directives :: !d
  }

-- | The language of the first Magix directive among the directive lines,
-- consuming nothing.
pLookAheadLanguage :: Parser Language
pLookAheadLanguage = lookAhead $ do
  skipMany $ notFollowedBy (chunk "#!magix") *> chunk "#!" *> takeWhileP Nothing (/= '\n') *> (void newline <|> eof)
  pMagixDirective <|> fail "no #!magix directive names the language of the script"

-- | The directive lines after the shebang, in any order: one Magix directive,
-- at most one Nixpkgs directive, and the directives of the language the Magix
-- directive names. No other directive line follows them.
pDirectiveLines :: Parser Directives
pDirectiveLines = do
  language <- pLookAheadLanguage
  case getDirectiveParser language of
    DirectiveParser pLanguageLine toLanguageDirectives -> do
      let pLine =
            choice
              [ MagixLine <$> try pMagixDirective,
                NixpkgsLine <$> try pNixpkgsDirective,
                LanguageLine <$> try pLanguageLine
              ]
          go ls = do
            start <- getOffset
            next <- optional $ lookAhead (chunk "#!")
            case next of
              Nothing -> pure ls
              Just _ -> do
                line <- pLine <* pLineEnd
                go =<< case line of
                  MagixLine l
                    | isJust ls.magix -> failAt start "a second #!magix directive; a script has one"
                    | otherwise -> pure ls {magix = Just l}
                  NixpkgsLine r
                    | isJust ls.nixpkgs -> failAt start "a second #!nixpkgs directive; a script has at most one"
                    | otherwise -> pure ls {nixpkgs = Just r}
                  LanguageLine d -> pure ls {directives = ls.directives <> d}
      ls <- go $ DirectiveLines Nothing Nothing mempty
      notFollowedBy $ space *> chunk "#!"
      pure $
        Directives
          { nixpkgsRef = ls.nixpkgs,
            language = toLanguageDirectives ls.directives
          }
  where
    failAt offset msg = setOffset offset *> fail msg

pDirectives :: Parser Directives
pDirectives = pShebang *> pLineEnd *> pDirectiveLines

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
