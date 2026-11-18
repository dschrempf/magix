-- |
-- Module      :  Magix.HashSpec
-- Description :  Golden values of the cache hashes
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 17 14:00:00 2026.
--
-- A changed hash rebuilds every cached script (`getMagixHash`) or orphans its
-- slot (`getSlotHash`). Changing a golden value is deliberate, and the
-- Changelog says so. `getMagixHash` includes the version of magix, so a
-- version bump changes its golden values.
module Magix.HashSpec
  ( spec,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (byteStringHex, toLazyByteString)
import Data.ByteString.Lazy.Char8 (unpack)
import Magix.BuildMode (Nixpkgs (..))
import Magix.Hash (MagixHashContents (..), getMagixHash, getSlotHash)
import Magix.LockedFlake (LockedFlake (LockedFlake))
import Test.Hspec (Spec, describe, it, shouldBe)

toHex :: ByteString -> String
toHex = unpack . toLazyByteString . byteStringHex

channel :: Nixpkgs
channel = NixpkgsPath "/nix/store/00000000000000000000000000000000-source"

-- | Under a UTF-8 locale, GHC decodes @ä@ to one character.
channelNonAscii :: Nixpkgs
channelNonAscii = NixpkgsPath "/home/dä/nixpkgs"

flake :: Nixpkgs
flake = NixpkgsFlake $ LockedFlake "github:NixOS/nixpkgs/d5cc6833999746cffdfe0598f7bc801a57b887a9" []

flakeNonAscii :: Nixpkgs
flakeNonAscii = NixpkgsFlake $ LockedFlake "path:/nix/store/00000000000000000000000000000000-dä" []

-- | Locked by @nix flake metadata@.
flakeWithNarHashes :: Nixpkgs
flakeWithNarHashes =
  NixpkgsFlake $
    LockedFlake
      "path:/home/user/flake?lastModified=1794979352&narHash=sha256-hTJPMh9Bwky2TlaWmLrkBHdUBmpxSCPRK%2BbmnFNpNQE%3D"
      ["sha256-hTJPMh9Bwky2TlaWmLrkBHdUBmpxSCPRK+bmnFNpNQE=", "sha256-DSelrdHzsmJ8bREZo0z/0Ik7E51F26FaOULDuWIc03I="]

script :: ByteString
script = "#!/usr/bin/env magix\n#!magix bash\n\necho Hello\n"

magixHash :: Nixpkgs -> ByteString -> String
magixHash m xs = toHex $ getMagixHash $ MagixHashContents {nixpkgs = m, scriptContents = xs}

golden :: String -> String -> String -> Spec
golden label actual expected = it label $ actual `shouldBe` expected

spec :: Spec
spec = do
  describe "getMagixHash" $ do
    golden "channel build, empty script" (magixHash channel "") $
      "9984bd22bb88d312eabf033bfaf2c90cb94ef03fd079df4f0ee86c97294e9b0b"
    golden "channel build" (magixHash channel script) $
      "a49fc45a56a5a6e21d5b64bffc5789e094e960bd66993fd2354310006dc42c64"
    golden "channel build, non-ASCII path" (magixHash channelNonAscii script) $
      "fab93020e5729eb890bd4ec04f02fb84580cb2ca463fab7c36ea952b1935a97f"
    golden "flake build, empty script" (magixHash flake "") $
      "d7dfb74f290cfface9d21744092b33edaab16e1b0d5977b83cdffab02f2c2d8d"
    golden "flake build" (magixHash flake script) $
      "e441fec2695c8d9664b7049df874936add3012cd92d18d722a75f017038f2a72"
    golden "flake build, non-ASCII reference" (magixHash flakeNonAscii script) $
      "73b26b747e5a35f0edd954c962842750721e36eb9fe3d5eecf253cc09973d2f3"
    golden "flake build, NAR hashes" (magixHash flakeWithNarHashes script) $
      "1a7573d055530b4fbbbe6073d3f4d4d1de9d3e089251c1036f0431ad8f28b020"

  describe "getSlotHash" $ do
    golden "ASCII path" (toHex $ getSlotHash "/home/user/bin/script") $
      "25d5c612fc5bc3e302a23db95c3d52491aa76812296687a8268656d9e671fabd"
    golden "non-ASCII path" (toHex $ getSlotHash "/home/dä/bin/script") $
      "311284e4e73403cfe13ce8ba5e6a8e327e9c2e23facb593a09d564da35700128"
