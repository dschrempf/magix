-- |
-- Module      :  Magix.NixpkgsPathSpec
-- Description :  Unit tests for
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 24 15:46:40 2024.
module Magix.NixpkgsPathSpec
  ( spec,
  )
where

import Control.Monad (replicateM)
import Magix.NixpkgsPath (NixPathEntry (..), getDefaultNixpkgsPath, pNixPath, resolveNixpkgs)
import Magix.Tools (parseS)
import System.Directory (createDirectory, createDirectoryLink, getTemporaryDirectory)
import System.FilePath ((</>))
import System.Random.Stateful (randomRIO)
import Test.Hspec (Spec, describe, it, shouldBe)

-- | Create a fresh, empty temporary directory.
getTemporaryTestDir :: IO FilePath
getTemporaryTestDir = do
  suffix <- replicateM 20 $ randomRIO ('a', 'z')
  tmp <- (</> ("magix-nixpath-" <> suffix)) <$> getTemporaryDirectory
  createDirectory tmp
  pure tmp

spec :: Spec
spec = do
  describe "pNixPath" $ do
    let parsesAs = parseS pNixPath
    it "parses prefixed entries" $ do
      "nixpkgs=/path/to/nixpkgs" `parsesAs` [Prefixed "nixpkgs" "/path/to/nixpkgs"]
      "nixpkgs=/path/to/nixpkgs:some=/other/path"
        `parsesAs` [Prefixed "nixpkgs" "/path/to/nixpkgs", Prefixed "some" "/other/path"]
      "other=/path/here:and=/another:nixpkgs=/path/to/nixpkgs"
        `parsesAs` [ Prefixed "other" "/path/here",
                     Prefixed "and" "/another",
                     Prefixed "nixpkgs" "/path/to/nixpkgs"
                   ]
    it "parses unprefixed directories" $ do
      "/etc/nix/inputs" `parsesAs` [Unprefixed "/etc/nix/inputs"]
      "/etc/nix/inputs:nixpkgs=/path/to/nixpkgs"
        `parsesAs` [Unprefixed "/etc/nix/inputs", Prefixed "nixpkgs" "/path/to/nixpkgs"]
    it "treats whitespace as a separator" $ do
      "nixpkgs=/path/to/nixpkgs some=/other/path"
        `parsesAs` [Prefixed "nixpkgs" "/path/to/nixpkgs", Prefixed "some" "/other/path"]
    it "parses the empty NIX_PATH as no entries" $ do
      "" `parsesAs` []
    it "splits an entry at its first '=' (values may contain '=')" $ do
      "nixpkgs=/a=b=c" `parsesAs` [Prefixed "nixpkgs" "/a=b=c"]
      "a=b=c:nixpkgs=/x"
        `parsesAs` [Prefixed "a" "b=c", Prefixed "nixpkgs" "/x"]
      -- Shared with Nix: an unprefixed path containing '=' is read as prefixed.
      "/weird=dir" `parsesAs` [Prefixed "/weird" "dir"]

  describe "getDefaultNixpkgsPath" $ do
    it "parses NIX_PATH into its entries" $ do
      getDefaultNixpkgsPath "other=/x:nixpkgs=/path/to/nixpkgs"
        `shouldBe` Right [Prefixed "other" "/x", Prefixed "nixpkgs" "/path/to/nixpkgs"]

  describe "resolveNixpkgs" $ do
    it "resolves a 'nixpkgs=' entry to its path" $ do
      res <- resolveNixpkgs (Prefixed "nixpkgs" "/path/to/nixpkgs")
      res `shouldBe` Just "/path/to/nixpkgs"

    it "resolves a 'nixpkgs=' entry whose value contains '='" $ do
      res <- resolveNixpkgs (Prefixed "nixpkgs" "/a=b=c")
      res `shouldBe` Just "/a=b=c"

    it "ignores other prefixed entries" $ do
      res <- resolveNixpkgs (Prefixed "other" "/path")
      res `shouldBe` Nothing
      -- A multi-'=' token like "a=b=c" is a prefixed 'a' entry: skipped, not an error.
      res' <- resolveNixpkgs (Prefixed "a" "b=c")
      res' `shouldBe` Nothing

    it "resolves an unprefixed directory containing 'nixpkgs'" $ do
      dir <- getTemporaryTestDir
      let target = dir </> "nixpkgs-target"
          link = dir </> "nixpkgs"
      createDirectory target
      createDirectoryLink target link
      res <- resolveNixpkgs (Unprefixed dir)
      res `shouldBe` Just link

    it "ignores an unprefixed directory without a 'nixpkgs' entry" $ do
      dir <- getTemporaryTestDir
      res <- resolveNixpkgs (Unprefixed dir)
      res `shouldBe` Nothing
