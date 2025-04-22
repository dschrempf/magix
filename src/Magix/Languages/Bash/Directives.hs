-- |
-- Module      :  Magix.Languages.Bash.Directives
-- Description :  Bash directives for Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Languages.Bash.Directives
  ( BashDirectives (..),
    pBashDirectives,
  )
where

import Data.Text (Text)
import Magix.Languages.Common.Directives
  ( Parser,
    pDirectiveWithValues,
    pManyDirectives,
  )
import Prelude hiding (readFile)

newtype BashDirectives = BashDirectives {_packages :: [Text]}
  deriving (Eq, Show, Semigroup, Monoid)

pBashDirectives :: Parser BashDirectives
pBashDirectives = pManyDirectives $ BashDirectives <$> pDirectiveWithValues "packages"
