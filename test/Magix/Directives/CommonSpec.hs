-- |
-- Module      :  Magix.Directives.CommonSpec
-- Description :  Tests for parsing common directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.Directives.CommonSpec
  ( spec,
  )
where

import Data.Either (isLeft, isRight)
import Magix.Directives.Common
  ( pDirectiveWithValue,
    pDirectiveWithValues,
    pLanguageDirectives,
    pMagixDirective,
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (chunk, parse)
import Prelude hiding (readFile)

spec :: Spec
spec = do
  describe "pDirectiveWithValue" $ do
    it "parses directive with one value" $ do
      parse (pDirectiveWithValue "foo" (chunk "bar")) "" "#!foo bar" `shouldBe` Right "bar"
      parse (pDirectiveWithValue "foo" (chunk "bar")) "" "#! foo bar" `shouldSatisfy` isLeft

  describe "pDirectiveWithValues" $ do
    it "parses directives with one or more values" $ do
      parse (pDirectiveWithValues "foo") "" "#!foo bar" `shouldBe` Right ["bar"]
      parse (pDirectiveWithValues "foo") "" "#!foo bar baz" `shouldBe` Right ["bar", "baz"]

    it "fails on directives without a value" $ do
      parse (pDirectiveWithValues "foo") "" "#!foo " `shouldSatisfy` isLeft
      parse (pDirectiveWithValues "foo") "" "#!foo" `shouldSatisfy` isLeft
      parse (pDirectiveWithValues "foo") "" "#!foo\n" `shouldSatisfy` isLeft
      parse (pDirectiveWithValues "foo") "" "#!\n" `shouldSatisfy` isLeft
      parse (pDirectiveWithValues "foo") "" "#! " `shouldSatisfy` isLeft

  describe "pMagixDirective" $ do
    it "parses sample Magix directives" $ do
      parse pMagixDirective "" "#!magix haskell" `shouldBe` Right "haskell"
      parse pMagixDirective "" "#!magix foo" `shouldBe` Right "foo"

    it "fails on wrong Magix directives" $ do
      parse pMagixDirective "" "#!magic haskell" `shouldSatisfy` isLeft
      parse pMagixDirective "" "#!magic" `shouldSatisfy` isLeft

  describe "pLanguageDirectives" $ do
    it "parses sample directive" $ do
      let p = pLanguageDirectives (pDirectiveWithValue "foo" (chunk "bar"))
      parse p "" "#!foo bar" `shouldSatisfy` isRight
      parse p "" "\n\n#!foo bar" `shouldSatisfy` isRight

-- TODO.
-- parse p "" "#!magix bash #!foo bar" `shouldSatisfy` isLeft
-- parse p "" "#!magix bash\n#!foo bar" `shouldSatisfy` isRight
