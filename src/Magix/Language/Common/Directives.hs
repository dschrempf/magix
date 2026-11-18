-- |
-- Module      :  Magix.Language.Common.Directives
-- Description :  Common tools for parsing directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Language.Common.Directives
  ( Parser,
    pValue,
    pDirectiveWithValue,
    pDirectiveWithValues,
    pLineEnd,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (void)
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec
  ( MonadParsec (eof, hidden, lookAhead),
    Parsec,
    chunk,
    sepEndBy1,
  )
import Text.Megaparsec.Char
  ( alphaNumChar,
    char,
    hspace,
    hspace1,
    newline,
    punctuationChar,
    symbolChar,
  )

type Parser = Parsec Void Text

pDirective :: Text -> Parser Text
pDirective d = chunk "#!" *> chunk d

pValue :: Parser Text
pValue = pack <$> some (alphaNumChar <|> punctuationChar <|> symbolChar)

pDirectiveWithValue :: Text -> Parser a -> Parser a
pDirectiveWithValue d p = pDirective d *> hspace1 *> p

pDirectiveWithValues :: Text -> Parser [Text]
pDirectiveWithValues d = pDirectiveWithValue d (sepEndBy1 pValue hspace)

-- | The end of a directive line: inline white space, then a newline or the end
-- of input. A carriage return fails with a message naming CRLF line endings.
pLineEnd :: Parser ()
pLineEnd = hspace *> (void newline <|> eof <|> pCarriageReturn)
  where
    pCarriageReturn =
      hidden (lookAhead (char '\r'))
        *> fail "a carriage return ends the directive line; magix needs LF, not CRLF, line endings"
