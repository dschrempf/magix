-- |
-- Module      :  Magix.FlakeRegistrySpec
-- Description :  Unit tests for Magix.FlakeRegistry
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 17 10:00:00 2026.
module Magix.FlakeRegistrySpec
  ( spec,
  )
where

import Data.ByteString.Char8 qualified as BS
import Data.Either (isLeft)
import Data.List (intercalate, isInfixOf)
import Data.Text (Text)
import Magix.Env (NixEnv (..))
import Magix.FlakeRegistry
  ( FlakeError (..),
    FlakeRef (..),
    Registry (..),
    RegistryEntry (..),
    RegistryFiles (..),
    getRegistryFiles,
    parseFlakeId,
    parseRegistry,
    readRegistry,
    resolveFlakeId,
    resolveFlakeRef,
  )
import Magix.Tools (getRandomTempDir)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

userFile, systemFile :: FilePath
userFile = "/user/registry.json"
systemFile = "/system/registry.json"

entry :: Text -> FlakeRef -> RegistryEntry
entry i = RegistryEntry (IndirectRef i)

-- | A version 2 registry mapping each id to a path or another id.
registryJson :: [(String, Either String FilePath)] -> String
registryJson xs =
  "{\"version\":2,\"flakes\":[" <> intercalate "," (map flake xs) <> "]}"
  where
    flake (i, t) =
      "{\"from\":{\"type\":\"indirect\",\"id\":\""
        <> i
        <> "\"},\"to\":"
        <> either indirect path t
        <> ",\"exact\":true}"
    indirect j = "{\"type\":\"indirect\",\"id\":\"" <> j <> "\"}"
    path p = "{\"type\":\"path\",\"path\":\"" <> p <> "\",\"narHash\":\"sha256-x\"}"

noEnv :: NixEnv (Maybe String)
noEnv = NixEnv Nothing Nothing Nothing Nothing

spec :: Spec
spec = do
  describe "parseRegistry" $ do
    it "parses path, indirect and other targets" $ do
      let json =
            "{\"version\":2,\"flakes\":["
              <> "{\"from\":{\"type\":\"indirect\",\"id\":\"a\"},\"to\":{\"type\":\"path\",\"path\":\"/nix/store/a\",\"rev\":\"r\"}},"
              <> "{\"from\":{\"type\":\"indirect\",\"id\":\"b\"},\"to\":{\"type\":\"indirect\",\"id\":\"a\"}},"
              <> "{\"from\":{\"type\":\"indirect\",\"id\":\"c\"},\"to\":{\"type\":\"github\",\"owner\":\"o\",\"repo\":\"r\"}},"
              <> "{\"from\":{\"type\":\"indirect\",\"id\":\"d\",\"ref\":\"main\"},\"to\":{\"type\":\"path\",\"path\":\"relative\"}}"
              <> "]}"
      fmap (.entries) (parseRegistry userFile (BS.pack json))
        `shouldBe` Right
          [ entry "a" (PathRef "/nix/store/a"),
            entry "b" (IndirectRef "a"),
            entry "c" (OtherRef "{\"owner\":\"o\",\"repo\":\"r\",\"type\":\"github\"}"),
            RegistryEntry
              (OtherRef "{\"id\":\"d\",\"ref\":\"main\",\"type\":\"indirect\"}")
              (OtherRef "{\"path\":\"relative\",\"type\":\"path\"}")
          ]

    it "fails on another version" $ do
      parseRegistry userFile "{\"version\":1,\"flakes\":[]}" `shouldSatisfy` isLeft

    it "fails on malformed JSON" $ do
      parseRegistry userFile "{\"version\":2," `shouldSatisfy` isLeft

  describe "parseFlakeId" $ do
    it "accepts a bare id" $ do
      parseFlakeId "nixpkgs" `shouldBe` Right "nixpkgs"
      parseFlakeId "nixpkgs-unfree_2" `shouldBe` Right "nixpkgs-unfree_2"

    it "rejects other flake references" $ do
      parseFlakeId "github:NixOS/nixpkgs" `shouldBe` Left (NotAnId "github:NixOS/nixpkgs")
      parseFlakeId "github" `shouldBe` Right "github"
      parseFlakeId "nixpkgs/nixos-unstable" `shouldSatisfy` isLeft
      parseFlakeId "nixpkgs?ref=x" `shouldSatisfy` isLeft
      parseFlakeId "1nixpkgs" `shouldSatisfy` isLeft
      parseFlakeId "" `shouldSatisfy` isLeft

  describe "resolveFlakeId" $ do
    let user = Registry userFile
        system = Registry systemFile
    it "resolves a path target" $ do
      resolveFlakeId [user [entry "nixpkgs" (PathRef "/nix/store/n")]] "nixpkgs"
        `shouldBe` Right "/nix/store/n"

    it "searches the user registry before the system registry" $ do
      resolveFlakeId
        [ user [entry "nixpkgs" (PathRef "/nix/store/u")],
          system [entry "nixpkgs" (PathRef "/nix/store/s")]
        ]
        "nixpkgs"
        `shouldBe` Right "/nix/store/u"

    it "falls back to the system registry" $ do
      resolveFlakeId
        [user [entry "other" (PathRef "/nix/store/o")], system [entry "nixpkgs" (PathRef "/nix/store/s")]]
        "nixpkgs"
        `shouldBe` Right "/nix/store/s"

    it "follows indirect targets, searching from the first registry" $ do
      resolveFlakeId
        [ user [entry "b" (PathRef "/nix/store/u")],
          system [entry "a" (IndirectRef "b"), entry "b" (PathRef "/nix/store/s")]
        ]
        "a"
        `shouldBe` Right "/nix/store/u"

    it "fails on a cycle" $ do
      resolveFlakeId [user [entry "a" (IndirectRef "b"), entry "b" (IndirectRef "a")]] "a"
        `shouldBe` Left (Cycle ["a", "b", "a"])
      resolveFlakeId [user [entry "a" (IndirectRef "a")]] "a"
        `shouldBe` Left (Cycle ["a", "a"])

    it "fails on a target other than a path" $ do
      resolveFlakeId [user [], system [entry "nixpkgs" (OtherRef "{}")]] "nixpkgs"
        `shouldBe` Left (NotAPath "nixpkgs" systemFile "{}")

    it "fails on an absent id" $ do
      resolveFlakeId [user [], system [entry "other" (PathRef "/nix/store/o")]] "nixpkgs"
        `shouldBe` Left (NotInRegistries "nixpkgs")

    it "ignores an entry whose source has a ref" $ do
      resolveFlakeId [user [RegistryEntry (OtherRef "{}") (PathRef "/nix/store/o")]] "nixpkgs"
        `shouldBe` Left (NotInRegistries "nixpkgs")

  describe "getRegistryFiles" $ do
    it "prefers NIX_CONFIG_HOME to XDG_CONFIG_HOME" $ do
      fs <- getRegistryFiles noEnv {nixConfigHome = Just "/n", xdgConfigHome = Just "/x"}
      fs.user `shouldBe` "/n/registry.json"

    it "uses XDG_CONFIG_HOME/nix" $ do
      fs <- getRegistryFiles noEnv {xdgConfigHome = Just "/x"}
      fs.user `shouldBe` "/x/nix/registry.json"

    it "uses NIX_CONF_DIR, unless empty" $ do
      fs <- getRegistryFiles noEnv {xdgConfigHome = Just "/x", nixConfDir = Just "/c"}
      fs.system `shouldBe` "/c/registry.json"
      fs' <- getRegistryFiles noEnv {xdgConfigHome = Just "/x", nixConfDir = Just ""}
      fs'.system `shouldBe` "/etc/nix/registry.json"

  describe "readRegistry" $ do
    it "reads a missing file as an empty registry" $ do
      dir <- getRandomTempDir
      res <- readRegistry (dir </> "registry.json")
      res `shouldBe` Right (Registry (dir </> "registry.json") [])

    it "fails on a malformed file" $ do
      dir <- getRandomTempDir
      writeFile (dir </> "registry.json") "{\"version\":2,"
      res <- readRegistry (dir </> "registry.json")
      res `shouldSatisfy` isLeft

  describe "resolveFlakeRef" $ do
    let withRegistries u s = do
          dir <- getRandomTempDir
          createDirectoryIfMissing True (dir </> "user")
          createDirectoryIfMissing True (dir </> "system")
          writeFile (dir </> "user" </> "registry.json") (registryJson u)
          writeFile (dir </> "system" </> "registry.json") (registryJson s)
          pure (dir, noEnv {nixConfigHome = Just (dir </> "user"), nixConfDir = Just (dir </> "system")})

    it "resolves through the registry files, the user one first" $ do
      (_, env) <- withRegistries [("nixpkgs", Left "b")] [("b", Right "/nix/store/s"), ("nixpkgs", Right "/nix/store/x")]
      res <- resolveFlakeRef env "nixpkgs"
      res `shouldBe` Right "/nix/store/s"

    it "fails on an absent id, naming the registry files" $ do
      (_, env) <- withRegistries [] [("other", Right "/nix/store/o")]
      res <- resolveFlakeRef env "nixpkgs"
      res `shouldSatisfy` either (\e -> all (`isInfixOf` e) ["'nixpkgs'", "user/registry.json", "system/registry.json"]) (const False)

    it "fails on a reference other than an id" $ do
      (_, env) <- withRegistries [] [("nixpkgs", Right "/nix/store/n")]
      res <- resolveFlakeRef env "github:NixOS/nixpkgs"
      res `shouldSatisfy` isLeft

    it "fails on a malformed registry" $ do
      (dir, env) <- withRegistries [] [("nixpkgs", Right "/nix/store/n")]
      writeFile (dir </> "user" </> "registry.json") "{"
      res <- resolveFlakeRef env "nixpkgs"
      res `shouldSatisfy` isLeft
