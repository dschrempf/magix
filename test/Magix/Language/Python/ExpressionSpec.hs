-- |
-- Module      :  Magix.Language.Python.ExpressionSpec
-- Description :  Tests for building Python Nix expressions
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:52:10 2024.
module Magix.Language.Python.ExpressionSpec
  ( spec,
  )
where

import Data.Text (Text)
import Magix.Language.Directives (LanguageDirectives (..))
import Magix.Language.Python.Directives (PythonDirectives (..))
import Magix.Tools (testExpression, testFlakeExpression)
import Test.Hspec (Spec)

pythonPackages :: [Text]
pythonPackages = ["fake", "inputs"]

pythonLanguageDirectives :: LanguageDirectives
pythonLanguageDirectives = PythonD $ PythonDirectives pythonPackages

spec :: Spec
spec = do
  testExpression pythonLanguageDirectives [pythonPackages]
  testFlakeExpression
    "github:NixOS/nixpkgs/nixos-unstable"
    pythonLanguageDirectives
    [pythonPackages]
