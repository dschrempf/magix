-- |
-- Module      :  Magix.Cache
-- Description :  Remove entries from the Magix cache
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Nov 16 16:23:00 2026.
module Magix.Cache
  ( recordInvokedPath,
    readInvokedPath,
    removeCacheContents,
    removeStaleEntries,
    removeSuperseded,
  )
where

import Control.Monad (forM_, unless)
import Magix.Config (Config (..))
import Magix.Paths (getInvokedPathLink)
import System.Directory
  ( createFileLink,
    doesDirectoryExist,
    doesPathExist,
    getSymbolicLinkTarget,
    listDirectory,
    pathIsSymbolicLink,
    removeDirectoryRecursive,
    removeFile,
    removePathForcibly,
  )
import System.FilePath (takeFileName, (</>))
import System.IO.Error (tryIOError)
import System.Log.Logger (Logger, Priority (..), logL)

-- | Remove a file, a symbolic link, or a directory with its contents.
removeEntry :: FilePath -> IO ()
removeEntry path = do
  isDir <- doesDirectoryExist path
  isLnk <- pathIsSymbolicLink path
  if isDir && not isLnk
    then removeDirectoryRecursive path
    else removeFile path

listEntries :: FilePath -> IO [FilePath]
listEntries dir = map (dir </>) <$> listDirectory dir

-- | Remove everything in the cache directory.
removeCacheContents :: FilePath -> IO ()
removeCacheContents cacheDir = listEntries cacheDir >>= mapM_ removeEntry

-- | Record the path the script is invoked by in its slot, as a symbolic link:
-- the link keeps the bytes of the path under any locale, and dangles once the
-- script is gone.
recordInvokedPath :: FilePath -> FilePath -> IO ()
recordInvokedPath slotDir invokedPath = do
  current <- readInvokedPath slotDir
  unless (current == Just invokedPath) $ do
    let link = getInvokedPathLink slotDir
    removePathForcibly link
    createFileLink invokedPath link

-- | The path recorded by `recordInvokedPath`, if any.
readInvokedPath :: FilePath -> IO (Maybe FilePath)
readInvokedPath slotDir =
  either (const Nothing) Just <$> tryIOError (getSymbolicLinkTarget $ getInvokedPathLink slotDir)

data CacheEntry
  = -- | A slot, with the path its script is invoked by.
    Slot !FilePath
  | -- | Anything else, such as an entry of the layout before slots.
    NoSlot

getCacheEntry :: FilePath -> IO CacheEntry
getCacheEntry entry = do
  isDir <- doesDirectoryExist entry
  isLnk <- pathIsSymbolicLink entry
  if isDir && not isLnk
    then maybe NoSlot Slot <$> readInvokedPath entry
    else pure NoSlot

-- | Remove the slots whose script is gone, and every entry that is no slot.
removeStaleEntries :: Logger -> FilePath -> IO ()
removeStaleEntries logger cacheDir = do
  let logD = logL logger DEBUG
  entries <- listEntries cacheDir
  forM_ entries $ \entry ->
    getCacheEntry entry >>= \case
      Slot invokedPath -> do
        -- Follows symbolic links, so a dangling link is stale.
        scriptExists <- doesPathExist invokedPath
        unless scriptExists $ do
          logD $ "Removing slot of missing script " <> invokedPath <> ": " <> entry
          removeEntry entry
      NoSlot -> do
        logD $ "Removing entry that is no slot: " <> entry
        removeEntry entry

-- | Remove the builds in the slot other than the current one.
removeSuperseded :: Logger -> Config -> IO ()
removeSuperseded logger cfg = do
  let logD = logL logger DEBUG
      keep =
        map
          takeFileName
          [cfg.lockPath, cfg.invokedPathLink, cfg.buildDir, cfg.resultLinkPath]
  entries <- listEntries cfg.slotDir
  forM_ entries $ \entry ->
    unless (takeFileName entry `elem` keep) $ do
      logD $ "Removing superseded entry: " <> entry
      removeEntry entry
