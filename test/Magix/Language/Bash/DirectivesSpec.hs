-- |
-- Module      :  Magix.Language.Bash.DirectivesSpec
-- Description :  Tests for parsing Bash directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.Language.Bash.DirectivesSpec
  ( spec,
  )
where

import Data.Text (Text, unlines)
import Magix.Language.Common.Directives (Parser)
import Magix.Language.Bash.Directives (BashDirectives (..), pBashDirective)
import Magix.Language.Tools (pManyDirectives)
import Magix.Tools (parseF, parseFContaining, parseS)
import Test.Hspec (Spec, describe, it)
import Prelude hiding (unlines)

empty :: Text
empty = " \t\n"

minimal :: Text
minimal = "#!packages jq"

multiple :: Text
multiple =
  unlines
    [ "#!packages a",
      "#!packages b c",
      "#!packages d e f"
    ]

pBashDirectives :: Parser BashDirectives
pBashDirectives = pManyDirectives pBashDirective

spec :: Spec
spec = do
  describe "pBashDirectives" $ do
    it "parses empty directives" $ do
      parseS pBashDirectives empty $ BashDirectives []

    it "parses minimal sample directives" $ do
      parseS pBashDirectives minimal $ BashDirectives ["jq"]

    it "parses more interesting sample directives with multiple declarations" $ do
      parseS pBashDirectives multiple $ BashDirectives ["a", "b", "c", "d", "e", "f"]

    it "fails on a directive line with an invalid tail" $ do
      parseF pBashDirectives "#!packages jq\DELfoo"
      parseF pBashDirectives "#!packages jq\DELfoo\n#!packages yq"

    it "fails on CRLF line endings, naming them" $ do
      parseFContaining pBashDirectives "#!packages jq\r\n" "CRLF"
      parseFContaining pBashDirectives "#!packages jq\r\n#!packages yq\r\n" "CRLF"

    it "fails on invalid directives" $ do
      parseF pBashDirectives $ "#!package foo" -- Wrong keyword
      parseF pBashDirectives $ "#!packages" -- Missing value
      parseF pBashDirectives $ "#!foo bar" -- Unknown directive
      parseF pBashDirectives $ "#!packages a\n#!foo b" -- Unknown directive mixed in
