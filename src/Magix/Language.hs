-- |
-- Module      :  Magix.Language
-- Description :  Languages supported by Magix
-- Copyright   :  2025 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Apr 21 14:10:46 2025.
module Magix.Language
  ( Language (..),
    getLanguageNameLowercase,
    pLanguage,
  )
where

import Data.Text (Text, pack, toLower)
import Magix.Languages.Directives (Parser)
import Text.Megaparsec (choice)
import Text.Megaparsec.Char (string)

data Language = Bash | Haskell | Python
  deriving (Eq, Show, Ord, Enum, Bounded)

getLanguageNameLowercase :: Language -> Text
getLanguageNameLowercase = toLower . pack . show

pLanguage :: Parser Language
pLanguage = choice $ map pAnyLanguage [minBound .. maxBound :: Language]
  where
    pAnyLanguage language = language <$ string (getLanguageNameLowercase language)
