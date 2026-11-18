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
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Magix.Env (NixEnv (..))
import Magix.FlakeRegistry
  ( FlakeError (..),
    FlakeRef (..),
    PathFlake (..),
    Registry (..),
    RegistryEntry (..),
    RegistryFiles (..),
    getRegistryFiles,
    parseFlakeId,
    parseRegistry,
    readRegistry,
    renderPathFlake,
    resolveFlakeId,
    resolveFlakeRef,
  )
import Magix.Tools (withTempDir)
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

pathRef :: FilePath -> FlakeRef
pathRef p = PathRef $ PathFlake p Map.empty

-- | Resolve an id to the path of its registry entry.
resolveId :: [Registry] -> Text -> Either FlakeError FilePath
resolveId rs = fmap (.path) . resolveFlakeId rs

noEnv :: NixEnv (Maybe String)
noEnv = NixEnv Nothing Nothing Nothing Nothing

spec :: Spec
spec = do
  describe "parseRegistry" $ do
    it "parses path, indirect and other targets" $ do
      let json =
            "{\"version\":2,\"flakes\":["
              <> "{\"from\":{\"type\":\"indirect\",\"id\":\"a\"},\"to\":{\"type\":\"path\",\"path\":\"/nix/store/a\",\"rev\":\"r\",\"lastModified\":17}},"
              <> "{\"from\":{\"type\":\"indirect\",\"id\":\"e\"},\"to\":{\"type\":\"path\",\"path\":\"/e\",\"x\":true}},"
              <> "{\"from\":{\"type\":\"indirect\",\"id\":\"b\"},\"to\":{\"type\":\"indirect\",\"id\":\"a\"}},"
              <> "{\"from\":{\"type\":\"indirect\",\"id\":\"c\"},\"to\":{\"type\":\"github\",\"owner\":\"o\",\"repo\":\"r\"}},"
              <> "{\"from\":{\"type\":\"indirect\",\"id\":\"d\",\"ref\":\"main\"},\"to\":{\"type\":\"path\",\"path\":\"relative\"}}"
              <> "]}"
      fmap (.entries) (parseRegistry userFile (BS.pack json))
        `shouldBe` Right
          [ entry "a" (PathRef $ PathFlake "/nix/store/a" $ Map.fromList [("lastModified", "17"), ("rev", "r")]),
            entry "e" (OtherRef "{\"path\":\"/e\",\"type\":\"path\",\"x\":true}"),
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

  describe "renderPathFlake" $ do
    it "renders the attributes as the query, sorted, percent-encoded like Nix" $ do
      -- From 'nix flake metadata nixpkgs' (Nix 2.35).
      let attrs =
            Map.fromList
              [ ("rev", "d5cc6833999746cffdfe0598f7bc801a57b887a9"),
                ("narHash", "sha256-Wpz/7x/YWnDIBu7yj6ixe8j6fIMsidd9Y+o1PqPF3iM="),
                ("lastModified", "1794537146")
              ]
      renderPathFlake (PathFlake "/nix/store/zakqk45ps6q9s4fjnjqkvp8hdnhkgd5f-source" attrs)
        `shouldBe` "path:/nix/store/zakqk45ps6q9s4fjnjqkvp8hdnhkgd5f-source?lastModified=1794537146&narHash=sha256-Wpz/7x/YWnDIBu7yj6ixe8j6fIMsidd9Y%2Bo1PqPF3iM%3D&rev=d5cc6833999746cffdfe0598f7bc801a57b887a9"

    it "renders a path without attributes without a query" $ do
      renderPathFlake (PathFlake "/nix/store/a b" Map.empty) `shouldBe` "path:/nix/store/a%20b"

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
      resolveId [user [entry "nixpkgs" (pathRef "/nix/store/n")]] "nixpkgs"
        `shouldBe` Right "/nix/store/n"

    it "searches the user registry before the system registry" $ do
      resolveId
        [ user [entry "nixpkgs" (pathRef "/nix/store/u")],
          system [entry "nixpkgs" (pathRef "/nix/store/s")]
        ]
        "nixpkgs"
        `shouldBe` Right "/nix/store/u"

    it "falls back to the system registry" $ do
      resolveId
        [user [entry "other" (pathRef "/nix/store/o")], system [entry "nixpkgs" (pathRef "/nix/store/s")]]
        "nixpkgs"
        `shouldBe` Right "/nix/store/s"

    it "follows indirect targets, searching from the first registry" $ do
      resolveId
        [ user [entry "b" (pathRef "/nix/store/u")],
          system [entry "a" (IndirectRef "b"), entry "b" (pathRef "/nix/store/s")]
        ]
        "a"
        `shouldBe` Right "/nix/store/u"

    it "fails on a cycle" $ do
      resolveId [user [entry "a" (IndirectRef "b"), entry "b" (IndirectRef "a")]] "a"
        `shouldBe` Left (Cycle ["a", "b", "a"])
      resolveId [user [entry "a" (IndirectRef "a")]] "a"
        `shouldBe` Left (Cycle ["a", "a"])

    it "fails on a target other than a path" $ do
      resolveId [user [], system [entry "nixpkgs" (OtherRef "{}")]] "nixpkgs"
        `shouldBe` Left (NotAPath "nixpkgs" systemFile "{}")

    it "fails on an absent id" $ do
      resolveId [user [], system [entry "other" (pathRef "/nix/store/o")]] "nixpkgs"
        `shouldBe` Left (NotInRegistries "nixpkgs")

    it "ignores an entry whose source has a ref" $ do
      resolveId [user [RegistryEntry (OtherRef "{}") (pathRef "/nix/store/o")]] "nixpkgs"
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
      withTempDir $ \dir -> do
        res <- readRegistry (dir </> "registry.json")
        res `shouldBe` Right (Registry (dir </> "registry.json") [])

    it "fails on a malformed file" $ do
      withTempDir $ \dir -> do
        writeFile (dir </> "registry.json") "{\"version\":2,"
        res <- readRegistry (dir </> "registry.json")
        res `shouldSatisfy` isLeft

  describe "resolveFlakeRef" $ do
    let withRegistries u s action =
          withTempDir $ \dir -> do
            createDirectoryIfMissing True (dir </> "user")
            createDirectoryIfMissing True (dir </> "system")
            writeFile (dir </> "user" </> "registry.json") (registryJson u)
            writeFile (dir </> "system" </> "registry.json") (registryJson s)
            action dir noEnv {nixConfigHome = Just (dir </> "user"), nixConfDir = Just (dir </> "system")}

    it "resolves through the registry files, the user one first" $ do
      withRegistries [("nixpkgs", Left "b")] [("b", Right "/nix/store/s"), ("nixpkgs", Right "/nix/store/x")] $ \_ env -> do
        res <- resolveFlakeRef env "nixpkgs"
        res `shouldBe` Right "/nix/store/s"

    it "fails on an absent id, naming the registry files" $ do
      withRegistries [] [("other", Right "/nix/store/o")] $ \_ env -> do
        res <- resolveFlakeRef env "nixpkgs"
        res `shouldSatisfy` either (\e -> all (`isInfixOf` e) ["'nixpkgs'", "user/registry.json", "system/registry.json"]) (const False)

    it "fails on a reference other than an id" $ do
      withRegistries [] [("nixpkgs", Right "/nix/store/n")] $ \_ env -> do
        res <- resolveFlakeRef env "github:NixOS/nixpkgs"
        res `shouldSatisfy` isLeft

    it "fails on a malformed registry" $ do
      withRegistries [] [("nixpkgs", Right "/nix/store/n")] $ \dir env -> do
        writeFile (dir </> "user" </> "registry.json") "{"
        res <- resolveFlakeRef env "nixpkgs"
        res `shouldSatisfy` isLeft
