-- |
-- Module      :  Magix.BuildSpec
-- Description :  Unit tests of builder
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 24 16:57:25 2024.
module Magix.BuildSpec
  ( spec,
  )
where

import Data.List (isSuffixOf, sort)
import Data.Text (Text)
import Data.Text.IO (readFile, writeFile)
import Magix.Build (BuildStatus (..), build, getBuildStatus, resolveResult, withBuildLock)
import Magix.Cache (readInvokedPath)
import Magix.Config (BuildMode (..), Config (..))
import Magix.Expression (getFlakeWrapper)
import Magix.Paths (getPendingBuildDir)
import Magix.StubNix (Call (..), Outcome (..), StubNix, getStoreDir, readCalls, setOutcome, withStubNix)
import Magix.Tools (setFakeHash, withFakeConfig)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    doesPathExist,
    executable,
    getPermissions,
    listDirectory,
    removeDirectoryRecursive,
  )
import System.Exit (ExitCode (..))
import System.FilePath (takeFileName, (</>))
import System.Log.Logger (Logger, Priority (..), getLogger, setLevel)
import System.Posix (createSymbolicLink)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldReturn, shouldThrow)
import Prelude hiding (readFile, writeFile)

script :: Text
script = "#!/usr/bin/env magix\n#!magix bash\necho Hello\n"

expr :: Text
expr = "{ }: null\n"

-- | A logger dropping the errors of failing builds.
getQuietLogger :: IO Logger
getQuietLogger = setLevel EMERGENCY <$> getLogger "Magix.BuildSpec"

-- | Run the action on a configuration of an existing script, with stubs of
-- Nix.
withScript :: (StubNix -> Config -> IO a) -> IO a
withScript action = withFakeConfig $ \c -> withStubNix c.scriptName $ \stub -> do
  writeFile c.scriptPath script
  action stub c

-- | Build as `runMagix` does, holding the lock.
buildLocked :: Config -> IO ()
buildLocked c = do
  logger <- getQuietLogger
  withBuildLock logger c $ build logger c expr

getResultLinks :: Config -> IO [FilePath]
getResultLinks c = filter ("-result" `isSuffixOf`) <$> listDirectory c.slotDir

spec :: Spec
spec = do
  describe "getBuildStatus" $ do
    it "detects non-existing builds" $ do
      withFakeConfig $ \c -> getBuildStatus c `shouldReturn` NeedToBuild

    it "detects dangling symbolic links to build result" $ do
      withFakeConfig $ \c -> do
        let rp = c.resultLinkPath
        createSymbolicLink "nonexistent" rp
        getBuildStatus c `shouldReturn` NeedToBuild

  describe "build" $ do
    it "writes the build directory, records the invoked path, and links the result" $
      withScript $ \stub c0 -> do
        let c = c0 {buildMode = ChannelBuild "/nixpkgs"}
        getBuildStatus c `shouldReturn` NeedToBuild
        buildLocked c
        readFile c.buildExprPath `shouldReturn` expr
        readFile (c.buildDir </> "script") `shouldReturn` script
        readInvokedPath c.slotDir `shouldReturn` Just c.invokedPath
        getBuildStatus c `shouldReturn` HasBeenBuilt
        readCalls stub
          `shouldReturn` [ Call
                             "nix-build"
                             ["-I", "nixpkgs=/nixpkgs", "--out-link", c.resultLinkPath, getPendingBuildDir c.buildDir]
                         ]

    it "builds a Flake through the wrapper" $
      withScript $ \stub c0 -> do
        let ref = "github:NixOS/nixpkgs/nixos-unstable"
            c = c0 {buildMode = FlakeBuild ref}
        buildLocked c
        wrapper <- getFlakeWrapper ref
        readFile (c.buildDir </> "flake.nix") `shouldReturn` wrapper
        getBuildStatus c `shouldReturn` HasBeenBuilt
        readCalls stub
          `shouldReturn` [ Call
                             "nix"
                             ["build", "--no-write-lock-file", "--out-link", c.resultLinkPath, getPendingBuildDir c.buildDir]
                         ]

    it "removes the build a build of other contents supersedes" $
      withScript $ \_ c -> do
        let c' = setFakeHash "otherHash" c
        buildLocked c
        buildLocked c'
        entries <- listDirectory c.slotDir
        sort entries
          `shouldBe` sort (map takeFileName [c'.lockPath, c'.invokedPathLink, c'.buildDir, c'.resultLinkPath])
        getBuildStatus c `shouldReturn` NeedToBuild

    it "fails, and keeps the previous build, when Nix fails" $
      withScript $ \stub c -> do
        let c' = setFakeHash "otherHash" c
        buildLocked c
        result <- resolveResult c
        setOutcome stub Fail
        buildLocked c' `shouldThrow` (== ExitFailure 1)
        getResultLinks c `shouldReturn` [takeFileName c.resultLinkPath]
        getBuildStatus c `shouldReturn` HasBeenBuilt
        doesDirectoryExist c.buildDir `shouldReturn` True
        doesPathExist c'.buildDir `shouldReturn` False
        doesPathExist (getPendingBuildDir c'.buildDir) `shouldReturn` False
        resolveResult c `shouldReturn` result

    it "fails, and keeps the previous build, when Nix fails a rebuild of the same contents" $
      withScript $ \stub c -> do
        buildLocked c
        result <- resolveResult c
        setOutcome stub Fail
        buildLocked c `shouldThrow` (== ExitFailure 1)
        getBuildStatus c `shouldReturn` HasBeenBuilt
        resolveResult c `shouldReturn` result
        readFile c.buildExprPath `shouldReturn` expr
        doesPathExist (getPendingBuildDir c.buildDir) `shouldReturn` False

    it "replaces the build of the same contents" $
      withScript $ \_ c -> do
        buildLocked c
        result <- resolveResult c
        writeFile (c.buildDir </> "stale") ""
        buildLocked c
        resolveResult c >>= (`shouldNotBe` result)
        doesPathExist (c.buildDir </> "stale") `shouldReturn` False
        getResultLinks c `shouldReturn` [takeFileName c.resultLinkPath]

  describe "resolveResult" $ do
    it "resolves the result link to the result" $
      withScript $ \stub c -> do
        buildLocked c
        result <- resolveResult c
        results <- listDirectory $ getStoreDir stub
        map (getStoreDir stub </>) results `shouldBe` [result]
        exe <- getPermissions (result </> "bin" </> c.scriptName)
        executable exe `shouldBe` True

  describe "withBuildLock" $ do
    it "creates the cache and slot directories, and returns the result of the action" $
      withFakeConfig $ \c -> do
        logger <- getQuietLogger
        removeDirectoryRecursive c.cacheDir
        withBuildLock logger c (doesDirectoryExist c.slotDir) `shouldReturn` True
        doesFileExist c.cacheLockPath `shouldReturn` True
        doesFileExist c.lockPath `shouldReturn` True
