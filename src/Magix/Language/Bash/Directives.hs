-- |
-- Module      :  Magix.Language.Bash.Directives
-- Description :  Bash directives for Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Language.Bash.Directives
  ( BashDirectives (..),
    pBashDirective,
  )
where

import Data.Text (Text)
import Magix.Language.Common.Directives
  ( Parser,
    pDirectiveWithValues,
  )
import Prelude hiding (readFile)

newtype BashDirectives = BashDirectives {_packages :: [Text]}
  deriving (Eq, Show, Semigroup, Monoid)

-- | One Bash directive line, without its end.
pBashDirective :: Parser BashDirectives
pBashDirective = BashDirectives <$> pDirectiveWithValues "packages"
