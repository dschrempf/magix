-- |
-- Module      :  Magix.Languages.Haskell.DirectivesSpec
-- Description :  Tests for parsing Haskell directives
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.Languages.Haskell.DirectivesSpec
  ( spec,
  )
where

import Data.Text (Text, unlines)
import Magix.Languages.Haskell.Directives (HaskellDirectives (..), pHaskellDirectives)
import Magix.Tools (parse')
import Test.Hspec (Spec, describe, it, shouldBe)
import Prelude hiding (unlines)

empty :: Text
empty = " \t\n"

minimal :: Text
minimal =
  unlines
    [ "#!haskellPackages bytestring",
      "#!ghcFlags -threaded"
    ]

multiple :: Text
multiple =
  unlines
    [ "#!haskellPackages a",
      "#!haskellPackages b c",
      "#!ghcFlags 1",
      "#!haskellPackages d e f",
      "#!ghcFlags 2",
      "#!ghcFlags 3 4"
    ]

spec :: Spec
spec = do
  describe "pHaskellDirectives" $ do
    it "parses empty directives" $ do
      parse' pHaskellDirectives empty `shouldBe` HaskellDirectives [] []

    it "parses minimal sample directives" $ do
      parse' pHaskellDirectives minimal `shouldBe` HaskellDirectives ["bytestring"] ["-threaded"]

    it "parses more interesting sample directives with multiple declarations" $ do
      parse' pHaskellDirectives multiple
        `shouldBe` HaskellDirectives ["a", "b", "c", "d", "e", "f"] ["1", "2", "3", "4"]
