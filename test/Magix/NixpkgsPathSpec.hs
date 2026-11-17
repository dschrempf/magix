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

import Magix.NixpkgsPath
  ( EntryError (..),
    NixPathEntry (..),
    NixPathTarget (..),
    getDefaultNixpkgsPath,
    pNixPath,
    resolveNixpkgs,
  )
import Magix.Tools (getRandomTempDir, parseS)
import System.Directory (createDirectory, createDirectoryLink)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

-- | A flake resolver knowing only the id 'nixpkgs'.
resolveFlake :: String -> IO (Either String FilePath)
resolveFlake "nixpkgs" = pure $ Right "/nix/store/x-source"
resolveFlake r = pure $ Left $ "unknown " <> r

-- | A flake resolver failing the test if called.
noFlake :: String -> IO (Either String FilePath)
noFlake r = expectationFailure ("resolved flake " <> r) >> pure (Left r)

spec :: Spec
spec = do
  describe "pNixPath" $ do
    let parsesAs = parseS pNixPath
    it "parses prefixed entries" $ do
      "nixpkgs=/path/to/nixpkgs" `parsesAs` [Prefixed "nixpkgs" (TargetPath "/path/to/nixpkgs")]
      "nixpkgs=/path/to/nixpkgs:some=/other/path"
        `parsesAs` [Prefixed "nixpkgs" (TargetPath "/path/to/nixpkgs"), Prefixed "some" (TargetPath "/other/path")]
      "other=/path/here:and=/another:nixpkgs=/path/to/nixpkgs"
        `parsesAs` [ Prefixed "other" (TargetPath "/path/here"),
                     Prefixed "and" (TargetPath "/another"),
                     Prefixed "nixpkgs" (TargetPath "/path/to/nixpkgs")
                   ]
    it "parses unprefixed directories" $ do
      "/etc/nix/inputs" `parsesAs` [Unprefixed (TargetPath "/etc/nix/inputs")]
      "/etc/nix/inputs:nixpkgs=/path/to/nixpkgs"
        `parsesAs` [Unprefixed (TargetPath "/etc/nix/inputs"), Prefixed "nixpkgs" (TargetPath "/path/to/nixpkgs")]
    it "treats whitespace as a separator" $ do
      "nixpkgs=/path/to/nixpkgs some=/other/path"
        `parsesAs` [Prefixed "nixpkgs" (TargetPath "/path/to/nixpkgs"), Prefixed "some" (TargetPath "/other/path")]
    it "parses the empty NIX_PATH as no entries" $ do
      "" `parsesAs` []
    it "splits an entry at its first '=' (values may contain '=')" $ do
      "nixpkgs=/a=b=c" `parsesAs` [Prefixed "nixpkgs" (TargetPath "/a=b=c")]
      "a=b=c:nixpkgs=/x"
        `parsesAs` [Prefixed "a" (TargetPath "b=c"), Prefixed "nixpkgs" (TargetPath "/x")]
      -- Shared with Nix: an unprefixed path containing '=' is read as prefixed.
      "/weird=dir" `parsesAs` [Prefixed "/weird" (TargetPath "dir")]
    it "keeps the colon of a 'flake:' target, like Nix" $ do
      "nixpkgs=flake:nixpkgs" `parsesAs` [Prefixed "nixpkgs" (TargetFlake "nixpkgs")]
      "a=/x:nixpkgs=flake:nixpkgs:b=/y"
        `parsesAs` [ Prefixed "a" (TargetPath "/x"),
                     Prefixed "nixpkgs" (TargetFlake "nixpkgs"),
                     Prefixed "b" (TargetPath "/y")
                   ]
      "flake:nixpkgs" `parsesAs` [Unprefixed (TargetFlake "nixpkgs")]
      -- Nix reads up to the next colon only.
      "nixpkgs=flake:github:NixOS/nixpkgs"
        `parsesAs` [Prefixed "nixpkgs" (TargetFlake "github"), Unprefixed (TargetPath "NixOS/nixpkgs")]
      -- The target is the text after the last '='.
      "a=b=flake:nixpkgs" `parsesAs` [Prefixed "a" (TargetPath "b=flake:nixpkgs")]
      "a=/x/flake:b" `parsesAs` [Prefixed "a" (TargetPath "/x/flake"), Unprefixed (TargetPath "b")]
    it "keeps the colon of a 'channel:' or URL target, like Nix" $ do
      "nixpkgs=channel:nixos-unstable:a=/x"
        `parsesAs` [Prefixed "nixpkgs" (TargetRemote "channel:nixos-unstable"), Prefixed "a" (TargetPath "/x")]
      "nixpkgs=https://example.org/n.tar.gz:a=/x"
        `parsesAs` [Prefixed "nixpkgs" (TargetRemote "https://example.org/n.tar.gz"), Prefixed "a" (TargetPath "/x")]
      "nixpkgs=https://host:8080/n.tar.gz"
        `parsesAs` [Prefixed "nixpkgs" (TargetRemote "https://host"), Unprefixed (TargetPath "8080/n.tar.gz")]
      -- No URL without '//'.
      "a=https:/x" `parsesAs` [Prefixed "a" (TargetPath "https"), Unprefixed (TargetPath "/x")]

  describe "getDefaultNixpkgsPath" $ do
    it "parses NIX_PATH into its entries" $ do
      getDefaultNixpkgsPath "other=/x:nixpkgs=/path/to/nixpkgs"
        `shouldBe` Right [Prefixed "other" (TargetPath "/x"), Prefixed "nixpkgs" (TargetPath "/path/to/nixpkgs")]

  describe "resolveNixpkgs" $ do
    it "resolves a 'nixpkgs=' entry to its path" $ do
      res <- resolveNixpkgs noFlake (Prefixed "nixpkgs" (TargetPath "/path/to/nixpkgs"))
      res `shouldBe` Right (Just "/path/to/nixpkgs")

    it "resolves a 'nixpkgs=' entry whose value contains '='" $ do
      res <- resolveNixpkgs noFlake (Prefixed "nixpkgs" (TargetPath "/a=b=c"))
      res `shouldBe` Right (Just "/a=b=c")

    it "ignores other prefixed entries" $ do
      res <- resolveNixpkgs noFlake (Prefixed "other" (TargetPath "/path"))
      res `shouldBe` Right Nothing
      -- A multi-'=' token like "a=b=c" is a prefixed 'a' entry: skipped, not an error.
      res' <- resolveNixpkgs noFlake (Prefixed "a" (TargetPath "b=c"))
      res' `shouldBe` Right Nothing

    it "fails on a 'nixpkgs=' entry with a 'channel:' or URL target" $ do
      let e = Prefixed "nixpkgs" (TargetRemote "channel:nixos-unstable")
      res <- resolveNixpkgs noFlake e
      res `shouldBe` Left (RemoteEntry e)

    it "fails on an unprefixed 'channel:' or URL target" $ do
      let e = Unprefixed (TargetRemote "https://example.org/n.tar.gz")
      res <- resolveNixpkgs noFlake e
      res `shouldBe` Left (RemoteEntry e)

    it "resolves a 'nixpkgs=' entry with a 'flake:' target" $ do
      res <- resolveNixpkgs resolveFlake (Prefixed "nixpkgs" (TargetFlake "nixpkgs"))
      res `shouldBe` Right (Just "/nix/store/x-source")

    it "fails on a 'nixpkgs=' entry with an unresolved 'flake:' target" $ do
      let e = Prefixed "nixpkgs" (TargetFlake "other")
      res <- resolveNixpkgs resolveFlake e
      res `shouldBe` Left (FlakeEntry e "unknown other")

    it "ignores other prefixed entries with a 'flake:' target, without resolving them" $ do
      res <- resolveNixpkgs noFlake (Prefixed "other" (TargetFlake "nixpkgs"))
      res `shouldBe` Right Nothing

    it "ignores other prefixed entries with a 'channel:' or URL target" $ do
      res <- resolveNixpkgs noFlake (Prefixed "other" (TargetRemote "channel:nixos-unstable"))
      res `shouldBe` Right Nothing

    it "resolves an unprefixed directory containing 'nixpkgs'" $ do
      dir <- getRandomTempDir
      let target = dir </> "nixpkgs-target"
          link = dir </> "nixpkgs"
      createDirectory target
      createDirectoryLink target link
      res <- resolveNixpkgs noFlake (Unprefixed (TargetPath dir))
      res `shouldBe` Right (Just link)

    it "ignores an unprefixed directory without a 'nixpkgs' entry" $ do
      dir <- getRandomTempDir
      res <- resolveNixpkgs noFlake (Unprefixed (TargetPath dir))
      res `shouldBe` Right Nothing

    it "resolves an unprefixed 'flake:' target containing 'nixpkgs'" $ do
      dir <- getRandomTempDir
      createDirectory (dir </> "nixpkgs")
      let resolveDir r = pure $ if r == "inputs" then Right dir else Left r
      res <- resolveNixpkgs resolveDir (Unprefixed (TargetFlake "inputs"))
      res `shouldBe` Right (Just (dir </> "nixpkgs"))
