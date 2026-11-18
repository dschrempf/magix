-- |
-- Module      :  Magix.Language.Haskell.Directives
-- Description :  Haskell directives for Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Language.Haskell.Directives
  ( HaskellDirectives (..),
    pHaskellDirective,
  )
where

import Control.Applicative (Alternative (..))
import Data.Text (Text)
import Magix.Language.Common.Directives
  ( Parser,
    pDirectiveWithValues,
  )
import Text.Megaparsec (try)
import Prelude hiding (readFile)

data HaskellDirectives = HaskellDirectives
  { _haskellPackages :: ![Text],
    _ghcFlags :: ![Text]
  }
  deriving (Eq, Show)

instance Semigroup HaskellDirectives where
  (<>) :: HaskellDirectives -> HaskellDirectives -> HaskellDirectives
  HaskellDirectives xs1 ys1 <> HaskellDirectives xs2 ys2 =
    HaskellDirectives (xs1 <> xs2) (ys1 <> ys2)

instance Monoid HaskellDirectives where
  mempty :: HaskellDirectives
  mempty = HaskellDirectives mempty mempty

pHaskellPackages :: Parser HaskellDirectives
pHaskellPackages = do
  pkgs <- pDirectiveWithValues "haskellPackages"
  pure $ HaskellDirectives pkgs []

pGhcFlags :: Parser HaskellDirectives
pGhcFlags = do
  flags <- pDirectiveWithValues "ghcFlags"
  pure $ HaskellDirectives [] flags

-- | One Haskell directive line, without its end.
pHaskellDirective :: Parser HaskellDirectives
pHaskellDirective = try pHaskellPackages <|> pGhcFlags
