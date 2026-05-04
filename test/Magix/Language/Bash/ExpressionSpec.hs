-- |
-- Module      :  Magix.Language.Bash.ExpressionSpec
-- Description :  Tests for building Bash Nix expressions
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:52:10 2024.
module Magix.Language.Bash.ExpressionSpec
  ( spec,
  )
where

import Data.Text (Text)
import Magix.Language.Bash.Directives (BashDirectives (..))
import Magix.Language.Directives (LanguageDirectives (..))
import Magix.Tools (testExpression, testFlakeExpression)
import Test.Hspec (Spec)

packages :: [Text]
packages = ["fake", "inputs"]

bashLanguageDirectives :: LanguageDirectives
bashLanguageDirectives = BashD $ BashDirectives packages

spec :: Spec
spec = do
  testExpression bashLanguageDirectives [packages]
  testFlakeExpression
    "github:NixOS/nixpkgs/nixos-unstable"
    bashLanguageDirectives
    [packages]
