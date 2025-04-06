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

import Data.Either (isLeft)
import Magix.Languages.Common.Directives
  ( pDirectiveWithValue,
    pDirectiveWithValues,
    pManyDirectives,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (MonadParsec (eof), chunk, parse)
import Prelude hiding (readFile)

spec :: Spec
spec = do
  describe "pDirectiveWithValue" $ do
    let foobar = pDirectiveWithValue "foo" (chunk "bar")
    it "parses directive with one value" $
      parse (foobar <* eof) "" "#!foo bar" `shouldBe` Right "bar"

    it "fails on directive with space" $
      parse foobar "" "#! foo bar" `shouldSatisfy` isLeft

    it "partially parses directives with more values" $ do
      parse foobar "" "#!foo bar baz" `shouldBe` Right "bar"
      parse (foobar <* eof) "" "#!foo bar baz" `shouldSatisfy` isLeft

    it "fails on bogus directives" $ do
      parse foobar "" "#!\n" `shouldSatisfy` isLeft
      parse foobar "" "#! " `shouldSatisfy` isLeft
      parse foobar "" " " `shouldSatisfy` isLeft
      parse foobar "" " bogus" `shouldSatisfy` isLeft

  let foos = pDirectiveWithValues "foo" <* eof
  describe "pDirectiveWithValues" $ do
    it "parses directives with one or more values" $ do
      parse foos "" "#!foo bar" `shouldBe` Right ["bar"]
      parse foos "" "#!foo bar baz" `shouldBe` Right ["bar", "baz"]

    it "fails on directives without a value" $ do
      parse foos "" "#!foo " `shouldSatisfy` isLeft
      parse foos "" "#!foo" `shouldSatisfy` isLeft
      parse foos "" "#!foo\n" `shouldSatisfy` isLeft

    it "fails on directives with space" $ do
      parse foos "" "#! foo bar" `shouldSatisfy` isLeft
      parse foos "" "#! foo" `shouldSatisfy` isLeft
      parse foos "" "#! foo bar baz" `shouldSatisfy` isLeft

    it "fails on bogus directives" $ do
      parse foos "" "#!\n" `shouldSatisfy` isLeft
      parse foos "" "#! " `shouldSatisfy` isLeft
      parse foos "" " " `shouldSatisfy` isLeft
      parse foos "" " bogus" `shouldSatisfy` isLeft

  describe "pManyDirectives" $ do
    let pDirectives = pManyDirectives (pDirectiveWithValues "foo") <* eof
    it "parses sample directives" $ do
      parse pDirectives "" "" `shouldBe` Right []
      parse pDirectives "" "#!foo 1 2 3" `shouldBe` Right ["1", "2", "3"]
      parse pDirectives "" "#!foo 1 2\n#!foo 4 5" `shouldBe` Right ["1", "2", "4", "5"]
      parse pDirectives "" "#!foo 1 2\n#!foo 4 5\n" `shouldBe` Right ["1", "2", "4", "5"]

    it "fails on erroneous directives" $ do
      parse pDirectives "" "s" `shouldSatisfy` isLeft
      parse pDirectives "" "\n" `shouldSatisfy` isLeft
      parse pDirectives "" "#!foo " `shouldSatisfy` isLeft
      parse pDirectives "" "#!foo 1 2\n\n#!foo 4 5" `shouldSatisfy` isLeft
