-- |
-- Module      :  Magix.Languages.Bash.ExpressionSpec
-- Description :  Tests for building Bash Nix expressions
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:52:10 2024.
module Magix.Languages.Bash.ExpressionSpec
  ( spec,
  )
where

import Data.Text (Text)
import Magix.Directives (Directives (..))
import Magix.Languages.Bash.Directives (BashDirectives (..))
import Magix.Tools (testExpression)
import Test.Hspec (Spec)

packages :: [Text]
packages = ["fake", "inputs"]

bashDirectives :: Directives
bashDirectives = BashD $ BashDirectives packages

spec :: Spec
spec = testExpression bashDirectives [packages]
