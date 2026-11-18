-- |
-- Module      :  Magix.LockedFlakeSpec
-- Description :  Unit tests of locking flake references
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Nov 18 10:00:00 2026.
module Magix.LockedFlakeSpec
  ( spec,
  )
where

import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.Foldable (for_)
import Data.Text (Text, unpack)
import Magix.LockedFlake (FlakeRefKind (..), Metadata (..), classifyFlakeRef, parseMetadata)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

rev :: Text
rev = "d5cc6833999746cffdfe0598f7bc801a57b887a9"

-- | Of a dirty Git flake without lock file, with the input @n@ (Nix 2.35).
dirtyGitMetadata :: ByteString
dirtyGitMetadata =
  "{\"fingerprint\":\"27fb\",\"lastModified\":0,\"locked\":{\"__final\":true,\"lastModified\":0,\"narHash\":\"sha256-DSelrdHzsmJ8bREZo0z/0Ik7E51F26FaOULDuWIc03I=\",\"type\":\"git\",\"url\":\"file:///tmp/mfg\"},\"locks\":{\"nodes\":{\"n\":{\"locked\":{\"lastModified\":1794979361,\"narHash\":\"sha256-hTJPMh9Bwky2TlaWmLrkBHdUBmpxSCPRK+bmnFNpNQE=\",\"path\":\"/tmp/mfl2\",\"type\":\"path\"},\"original\":{\"path\":\"/tmp/mfl2\",\"type\":\"path\"}},\"root\":{\"inputs\":{\"n\":\"n\"}}},\"root\":\"root\",\"version\":7},\"original\":{\"type\":\"git\",\"url\":\"file:///tmp/mfg\"},\"originalUrl\":\"git+file:///tmp/mfg\",\"path\":\"/nix/store/7mmxfw516cm399l36w3yk4mg23rzhm5w-source\",\"resolved\":{\"type\":\"git\",\"url\":\"file:///tmp/mfg\"},\"resolvedUrl\":\"git+file:///tmp/mfg\",\"url\":\"git+file:///tmp/mfg\"}"

-- | Of @nixpkgs@ (Nix 2.35); the flake has no inputs.
nixpkgsMetadata :: ByteString
nixpkgsMetadata =
  "{\"locked\":{\"__final\":true,\"lastModified\":1794537146,\"narHash\":\"sha256-Wpz/7x/YWnDIBu7yj6ixe8j6fIMsidd9Y+o1PqPF3iM=\",\"path\":\"/nix/store/zakqk45ps6q9s4fjnjqkvp8hdnhkgd5f-source\",\"rev\":\"d5cc6833999746cffdfe0598f7bc801a57b887a9\",\"type\":\"path\"},\"locks\":{\"nodes\":{\"root\":{}},\"root\":\"root\",\"version\":7},\"url\":\"path:/nix/store/zakqk45ps6q9s4fjnjqkvp8hdnhkgd5f-source?lastModified=1794537146&narHash=sha256-Wpz/7x/YWnDIBu7yj6ixe8j6fIMsidd9Y%2Bo1PqPF3iM%3D&rev=d5cc6833999746cffdfe0598f7bc801a57b887a9\"}"

classifies :: FlakeRefKind -> [Text] -> Spec
classifies kind refs =
  for_ refs $ \r -> it (unpack r) $ classifyFlakeRef r `shouldBe` kind

spec :: Spec
spec = do
  describe "classifyFlakeRef" $ do
    describe "a flake id" $ do
      it "nixpkgs" $ classifyFlakeRef "nixpkgs" `shouldBe` FlakeId "nixpkgs"
      it "flake:nixpkgs" $ classifyFlakeRef "flake:nixpkgs" `shouldBe` FlakeId "nixpkgs"

    describe "fixed by its text" $
      classifies
        Fixed
        [ "path:/nix/store/zakqk45ps6q9s4fjnjqkvp8hdnhkgd5f-source",
          "/nix/store/zakqk45ps6q9s4fjnjqkvp8hdnhkgd5f-source",
          "github:NixOS/nixpkgs/" <> rev,
          "github:NixOS/nixpkgs/" <> rev <> "?dir=lib",
          "gitlab:o/r/" <> rev,
          "sourcehut:~o/r/" <> rev,
          "github:NixOS/nixpkgs?rev=" <> rev,
          "git+file:///home/u/flake?ref=main&rev=" <> rev,
          "path:/home/u/flake?narHash=sha256-x",
          "flake:nixpkgs/nixos-unstable?rev=" <> rev
        ]

    describe "mutable" $
      classifies
        Mutable
        [ "nixpkgs/nixos-unstable",
          "flake:nixpkgs/nixos-unstable",
          "github:NixOS/nixpkgs",
          "github:NixOS/nixpkgs/nixos-unstable",
          "github:NixOS/nixpkgs/d5cc683",
          "github:NixOS/nixpkgs?ref=nixos-unstable",
          "github:NixOS/nixpkgs?dir=" <> rev,
          "gitlab:o/r",
          "git+https://example.org/r?dir=x/" <> rev,
          "path:/home/u/flake",
          "/home/u/flake",
          "path:/home/u/nix/store/x",
          "git+file:///home/u/flake",
          "https://example.org/nixpkgs.tar.gz",
          "path:.",
          "."
        ]

  describe "parseMetadata" $ do
    it "reads the locked reference and the NAR hashes of a flake and its inputs" $
      parseMetadata dirtyGitMetadata
        `shouldBe` Right
          ( Metadata
              "git+file:///tmp/mfg"
              "sha256-DSelrdHzsmJ8bREZo0z/0Ik7E51F26FaOULDuWIc03I="
              ["sha256-hTJPMh9Bwky2TlaWmLrkBHdUBmpxSCPRK+bmnFNpNQE="]
          )

    it "reads a flake without inputs" $
      parseMetadata nixpkgsMetadata
        `shouldBe` Right
          ( Metadata
              "path:/nix/store/zakqk45ps6q9s4fjnjqkvp8hdnhkgd5f-source?lastModified=1794537146&narHash=sha256-Wpz/7x/YWnDIBu7yj6ixe8j6fIMsidd9Y%2Bo1PqPF3iM%3D&rev=d5cc6833999746cffdfe0598f7bc801a57b887a9"
              "sha256-Wpz/7x/YWnDIBu7yj6ixe8j6fIMsidd9Y+o1PqPF3iM="
              []
          )

    it "fails without a NAR hash" $
      parseMetadata "{\"url\":\"u\",\"locked\":{},\"locks\":{\"nodes\":{}}}" `shouldSatisfy` isLeft
