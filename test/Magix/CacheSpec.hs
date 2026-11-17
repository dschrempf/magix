-- |
-- Module      :  Magix.CacheSpec
-- Description :  Unit tests of cache removal
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov 16 16:24:30 2026.
module Magix.CacheSpec
  ( spec,
  )
where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Data.List (sort)
import Magix.Build (withBuildLock)
import Magix.Cache
  ( readInvokedPath,
    recordInvokedPath,
    removeCacheContents,
    removeStaleEntries,
    removeSuperseded,
  )
import Magix.Config (Config (..))
import Magix.Hash (getSlotHash)
import Magix.Paths (getSlotDir)
import Magix.Tools (withFakeConfig)
import System.Directory
  ( createDirectory,
    createDirectoryIfMissing,
    doesDirectoryExist,
    doesPathExist,
    listDirectory,
  )
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Log.Logger (Logger, getLogger)
import System.Posix (createSymbolicLink)
import System.Timeout (timeout)
import Test.Hspec (Expectation, Spec, describe, it, shouldBe, shouldReturn)

-- | Create a slot for a script invoked by the given path, which is left alone.
createSlot :: FilePath -> FilePath -> IO FilePath
createSlot cacheDir invokedPath = do
  let slotDir = getSlotDir cacheDir (getSlotHash invokedPath) (takeFileName invokedPath)
  createDirectoryIfMissing True slotDir
  recordInvokedPath slotDir invokedPath
  pure slotDir

-- | A path holding a byte that is no valid UTF-8, which decodes to the same
-- escaped character under every locale.
nonUtf8Path :: FilePath -> FilePath
nonUtf8Path dir = dir </> "d\xDCE4"

-- | Cleaning waits for a build holding the locks, here at the start of a first
-- build: a slot without a recorded path, which cleaning removes afterwards.
waitsForBuild :: (Logger -> FilePath -> IO ()) -> Expectation
waitsForBuild clean = do
  logger <- getLogger "test"
  withFakeConfig $ \c -> do
    building <- newEmptyMVar
    finish <- newEmptyMVar
    cleaned <- newEmptyMVar
    _ <- forkIO $ withBuildLock logger c $ putMVar building () >> takeMVar finish
    timeout second (takeMVar building) `shouldReturn` Just ()
    _ <- forkIO $ clean logger c.cacheDir >> putMVar cleaned ()
    threadDelay $ second `div` 10
    doesDirectoryExist c.slotDir `shouldReturn` True
    putMVar finish ()
    timeout second (takeMVar cleaned) `shouldReturn` Just ()
    doesDirectoryExist c.slotDir `shouldReturn` False
  where
    second = 1000000

spec :: Spec
spec = do
  describe "recordInvokedPath" $ do
    it "keeps a path that is no valid UTF-8" $ do
      withFakeConfig $ \c -> do
        let invokedPath = nonUtf8Path $ takeDirectory c.cacheDir
        recordInvokedPath c.slotDir invokedPath
        readInvokedPath c.slotDir `shouldReturn` Just invokedPath

  describe "removeSuperseded" $ do
    it "keeps the current build and removes the others" $ do
      logger <- getLogger "test"
      withFakeConfig $ \c -> do
        writeFile c.lockPath ""
        recordInvokedPath c.slotDir c.invokedPath
        createDirectory c.buildDir
        createSymbolicLink "store" c.resultLinkPath
        createDirectory $ c.slotDir </> "00-build"
        createSymbolicLink "store" $ c.slotDir </> "00-result"
        removeSuperseded logger c
        entries <- listDirectory c.slotDir
        sort entries
          `shouldBe` sort
            (map takeFileName [c.lockPath, c.invokedPathLink, c.buildDir, c.resultLinkPath])

  describe "removeCacheContents" $ do
    it "accepts a missing cache directory" $ do
      logger <- getLogger "test"
      withFakeConfig $ \c -> do
        let cacheDir = takeDirectory c.cacheDir </> "missing"
        removeCacheContents logger cacheDir
        doesPathExist cacheDir `shouldReturn` False

    it "removes everything but the cache lock" $ do
      logger <- getLogger "test"
      withFakeConfig $ \c -> do
        _ <- createSlot c.cacheDir $ takeDirectory c.cacheDir </> "missing"
        writeFile (c.cacheDir </> "00-old.lock") ""
        writeFile c.cacheLockPath ""
        removeCacheContents logger c.cacheDir
        listDirectory c.cacheDir `shouldReturn` [takeFileName c.cacheLockPath]

    it "waits for a running build" $ waitsForBuild removeCacheContents

  describe "removeStaleEntries" $ do
    it "accepts a missing cache directory" $ do
      logger <- getLogger "test"
      withFakeConfig $ \c -> do
        let cacheDir = takeDirectory c.cacheDir </> "missing"
        removeStaleEntries logger cacheDir
        doesPathExist cacheDir `shouldReturn` False

    it "keeps slots of existing scripts and removes everything else" $ do
      logger <- getLogger "test"
      withFakeConfig $ \c -> do
        let tmp = takeDirectory c.cacheDir
            present = tmp </> "present"
            dangling = tmp </> "dangling"
        writeFile present ""
        createSymbolicLink (tmp </> "nonexistent") dangling
        kept <- createSlot c.cacheDir present
        _ <- createSlot c.cacheDir dangling
        _ <- createSlot c.cacheDir (tmp </> "missing")
        -- Entries of the layout before slots.
        createDirectory $ c.cacheDir </> "00-old-build"
        createSymbolicLink "store" $ c.cacheDir </> "00-old-result"
        writeFile (c.cacheDir </> "00-old.lock") ""
        -- `c.slotDir` exists too, without a recorded path.
        writeFile c.cacheLockPath ""
        removeStaleEntries logger c.cacheDir
        entries <- listDirectory c.cacheDir
        sort entries `shouldBe` sort (map takeFileName [kept, c.cacheLockPath])

    it "keeps the slot of an existing script whose path is no valid UTF-8" $ do
      logger <- getLogger "test"
      withFakeConfig $ \c -> do
        let present = nonUtf8Path $ takeDirectory c.cacheDir
        writeFile present ""
        kept <- createSlot c.cacheDir present
        removeStaleEntries logger c.cacheDir
        entries <- listDirectory c.cacheDir
        sort entries `shouldBe` sort (map takeFileName [kept, c.cacheLockPath])

    it "waits for a running build" $ waitsForBuild removeStaleEntries
