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
  let foos = pDirectiveWithValues "foo"
  describe "pDirectiveWithValue" $ do
    let foobar = pDirectiveWithValue "foo" (chunk "bar")
    it "parses directive with one value" $ do
      parse foobar "" "#!foo bar" `shouldBe` Right "bar"

    it "fails on directive with space" $
      parse foobar "" "#! foo bar" `shouldSatisfy` isLeft

  describe "pDirectiveWithValues" $ do
    it "parses directives with one or more values" $ do
      parse foos "" "#!foo bar" `shouldBe` Right ["bar"]
      parse foos "" "#!foo bar baz" `shouldBe` Right ["bar", "baz"]

    it "fails on directives without a value" $ do
      parse foos "" "#!foo " `shouldSatisfy` isLeft
      parse foos "" "#!foo" `shouldSatisfy` isLeft
      parse foos "" "#!foo\n" `shouldSatisfy` isLeft
      parse foos "" "#!\n" `shouldSatisfy` isLeft
      parse foos "" "#! " `shouldSatisfy` isLeft

  describe "pManyDirectives" $ do
    let pDirectives = pManyDirectives foos <* eof
    it "parses sample directives" $ do
      parse pDirectives "" "" `shouldBe` Right []
      parse pDirectives "" "#!foo 1 2 3" `shouldBe` Right ["1", "2", "3"]
      parse pDirectives "" "#!foo 1 2\n#!foo 4 5" `shouldBe` Right ["1", "2", "4", "5"]

    it "fails on erroneous directives" $ do
      parse pDirectives "" "s" `shouldSatisfy` isLeft
      parse pDirectives "" "\n" `shouldSatisfy` isLeft
      parse pDirectives "" "#!foo " `shouldSatisfy` isLeft
      parse pDirectives "" "#!foo 1 2\n\n#!foo 4 5" `shouldSatisfy` isLeft
