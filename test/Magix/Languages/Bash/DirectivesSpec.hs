-- |
-- Module      :  Magix.Languages.Bash.DirectivesSpec
-- Description :  Tests for parsing Bash directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.Languages.Bash.DirectivesSpec
  ( spec,
  )
where

import Data.Text (Text, unlines)
import Magix.Languages.Bash.Directives (BashDirectives (..), pBashDirectives)
import Magix.Tools (parse')
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (unlines)

empty :: Text
empty = "#!magix bash"

minimal :: Text
minimal =
  unlines
    [ "#!magix bash",
      "#!packages jq"
    ]

multiple :: Text
multiple =
  unlines
    [ "#!magix bash",
      "#!packages a",
      "#!packages b c",
      "#!packages d e f"
    ]

spec :: Spec
spec = do
  describe "pBashDirectives" $ do
    it "parses empty directives" $ do
      parse' pBashDirectives empty `shouldBe` BashDirectives []

    it "parses minimal sample directives" $ do
      parse' pBashDirectives minimal `shouldBe` BashDirectives ["jq"]

    it "parses more interesting sample directives with multiple declarations" $ do
      parse' pBashDirectives multiple `shouldBe` BashDirectives ["a", "b", "c", "d", "e", "f"]
