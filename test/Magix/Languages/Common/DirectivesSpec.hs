-- |
-- Module      :  Magix.Languages.Common.DirectivesSpec
-- Description :  Tests for parsing common directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.Languages.Common.DirectivesSpec
  ( spec,
  )
where

import Magix.Languages.Common.Directives
  ( pDirectiveWithValue,
    pDirectiveWithValues,
    pManyDirectives,
  )
import Magix.Tools (parseF, parseS)
import Test.Hspec (Spec, describe, it)
import Text.Megaparsec (MonadParsec (eof), chunk)
import Prelude hiding (readFile)

spec :: Spec
spec = do
  describe "pDirectiveWithValue" $ do
    let foobar = pDirectiveWithValue "foo" (chunk "bar")
    it "parses directive with one value" $
      parseS (foobar <* eof) "#!foo bar" "bar"

    it "fails on directive with space" $
      parseF foobar "#! foo bar"

    it "partially parses directives with more values" $ do
      parseS foobar "#!foo bar baz" "bar"
      parseF (foobar <* eof) "#!foo bar baz"

    it "fails on bogus directives" $ do
      parseF foobar "#!\n"
      parseF foobar "#! "
      parseF foobar " "
      parseF foobar " bogus"

  let foos = pDirectiveWithValues "foo" <* eof
  describe "pDirectiveWithValues" $ do
    it "parses directives with one or more values" $ do
      parseS foos "#!foo bar" ["bar"]
      parseS foos "#!foo bar baz" ["bar", "baz"]

    it "fails on directives without a value" $ do
      parseF foos "#!foo "
      parseF foos "#!foo"
      parseF foos "#!foo\n"

    it "fails on directives with space" $ do
      parseF foos "#! foo bar"
      parseF foos "#! foo"
      parseF foos "#! foo bar baz"

    it "fails on bogus directives" $ do
      parseF foos "#!\n"
      parseF foos "#! "
      parseF foos " "
      parseF foos " bogus"

  describe "pManyDirectives" $ do
    let pDirectives = pManyDirectives (pDirectiveWithValues "foo") <* eof
    it "parses sample directives" $ do
      parseS pDirectives "" []
      parseS pDirectives "#!foo 1 2 3" ["1", "2", "3"]
      parseS pDirectives "#!foo 1 2\n#!foo 4 5" ["1", "2", "4", "5"]
      parseS pDirectives "#!foo 1 2\n#!foo 4 5\n" ["1", "2", "4", "5"]

    it "fails on erroneous directives" $ do
      parseF pDirectives "s"
      parseF pDirectives "\n"
      parseF pDirectives "#!foo "
      parseF pDirectives "#!foo 1 2\n\n#!foo 4 5"
