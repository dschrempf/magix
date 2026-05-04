-- |
-- Module      :  Magix.Languages.Haskell.ExpressionSpec
-- Description :  Tests for building Bash Nix expressions
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:52:10 2024.
module Magix.Languages.Haskell.ExpressionSpec
  ( spec,
  )
where

import Data.Text (Text)
import Magix.Languages.Directives (LanguageDirectives (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Magix.Tools (testExpression, testFlakeExpression)
import Test.Hspec (Spec)

haskellPackages :: [Text]
haskellPackages = ["fake", "packages"]

ghcFlags :: [Text]
ghcFlags = ["fake", "flags"]

haskellLanguageDirectives :: LanguageDirectives
haskellLanguageDirectives = HaskellD $ HaskellDirectives haskellPackages ghcFlags

spec :: Spec
spec = do
  testExpression haskellLanguageDirectives [haskellPackages, ghcFlags]
  testFlakeExpression
    "github:NixOS/nixpkgs/nixos-unstable"
    haskellLanguageDirectives
    [haskellPackages, ghcFlags]
