-- |
-- Module      :  Main
-- Description :  Magically run and cache compiled scripts
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:12:06 2024.
module Main
  ( main,
  )
where

import Control.Exception (onException, throwIO)
import Control.Monad (forM_)
import Data.ByteString (readFile)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8')
import Magix
  ( BuildStatus (..),
    CleanOptions (..),
    Command (..),
    Config,
    Directives (..),
    LanguageDirectives,
    Options (..),
    Rebuild (..),
    Verbosity (..),
    build,
    getBuildStatus,
    getCacheDir,
    getCommand,
    getConfig,
    getDirectives,
    getNixExpression,
    runScript,
    withBuildLock,
  )
import System.Console.ANSI
  ( Color (Green, Red, White),
    ColorIntensity (Dull),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    setSGRCode,
  )
import System.Directory
  ( doesDirectoryExist,
    listDirectory,
    pathIsSymbolicLink,
    removeDirectoryRecursive,
    removeFile,
  )
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO (Handle, stderr)
import System.Log.Formatter (tfLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (GenericHandler, streamHandler)
import System.Log.Logger
  ( Logger,
    Priority (..),
    getLogger,
    logL,
    removeHandler,
    rootLoggerName,
    saveGlobalLogger,
    setHandlers,
    setLevel,
    updateGlobalLogger,
  )
import Prelude hiding (readFile)

withFormatter :: GenericHandler Handle -> GenericHandler Handle
withFormatter handler = setFormatter handler formatter
  where
    -- Sadly, coloring log messages is a pain.
    formatter handler' (prio, msg) =
      tfLogFormatter "%F %X.%6q %Z" format handler' (prio, msg)
      where
        color = case prio of
          DEBUG -> White
          INFO -> Green
          WARNING -> Red
          ERROR -> Red
          _unused -> White
        align = case prio of
          DEBUG -> "  "
          INFO -> "   "
          WARNING -> ""
          ERROR -> "  "
          _unused -> ""
        setC = setSGRCode [SetColor Foreground Dull color, SetConsoleIntensity BoldIntensity]
        rstC = setSGRCode [Reset]
        format = setC <> "[$time $loggername $prio]" <> rstC <> align <> " $msg"

mainLogger :: String
mainLogger = "Magix.Main"

setupLogger :: Verbosity -> IO Logger
setupLogger v = do
  let prio = case v of
        Info -> INFO
        Debug -> DEBUG
  updateGlobalLogger rootLoggerName removeHandler
  stderrHandler <- withFormatter <$> streamHandler stderr prio
  logger <- setHandlers [stderrHandler] . setLevel prio <$> getLogger mainLogger
  saveGlobalLogger logger
  pure logger

newBuild :: Config -> LanguageDirectives -> IO ()
newBuild conf dirs = do
  logger <- getLogger mainLogger
  let logD = logL logger DEBUG
  logD "Getting Magix Nix expression"
  expr <- getNixExpression conf dirs
  logD $ "Magix Nix expression:\n" <> unpack expr
  logD "Building Magix Nix expression"
  build logger conf expr
  logD "Built Magix Nix expression"

cleanCache :: CleanOptions -> IO ()
cleanCache opts = do
  logger <- setupLogger opts.verbosity
  let logI = logL logger INFO
  cacheDir <- getCacheDir opts.cachePath
  logI $ "Cleaning Magix cache: " <> cacheDir
  removeDirectoryContents cacheDir
  logI "Done"
  exitSuccess
  where
    removeDirectoryContents :: FilePath -> IO ()
    removeDirectoryContents dir = do
      contents <- listDirectory dir
      forM_ contents $ \name -> do
        let fullPath = dir </> name
        isDir <- doesDirectoryExist fullPath
        isLnk <- pathIsSymbolicLink fullPath
        if isDir && not isLnk
          then removeDirectoryRecursive fullPath
          else removeFile fullPath

runMagix :: Options -> IO ()
runMagix opts = do
  logger <- setupLogger opts.verbosity
  let logD = logL logger DEBUG
      logI = logL logger INFO
      logE = logL logger ERROR
  logD $ "Options are: " <> show opts

  let path = scriptPath opts
  logD $ "Reading script at path: " <> path
  scriptBytes <-
    readFile path `onException` do
      logE $ "Could not read file: " <> path
      exitFailure

  logD "Decoding script file"
  scriptTxt <- case decodeUtf8' scriptBytes of
    Left e -> logE "Failed decoding script file" >> throwIO e
    Right t -> logD "Successfully decoded script file" >> pure t

  logD "Parsing directives"
  directives <- case getDirectives path scriptTxt of
    Left e -> logE "Failed parsing directives" >> throwIO e
    Right result -> do
      logD $ "Successfully parsed directives: " <> show result
      pure result

  conf <- getConfig opts scriptBytes directives.nixpkgsRef
  logD $ "Magix configuration is " <> show conf

  case forceBuild opts of
    ForceBuild -> do
      logI "Forcing build"
      withBuildLock logger conf $ newBuild conf directives.language
    ReuseBuildIfAvailable -> do
      logD "Reusing build if available"
      logD "Checking build status"
      withBuildLock logger conf $ do
        buildStatus <- getBuildStatus conf
        case buildStatus of
          HasBeenBuilt ->
            logI "Script has already been built"
          NeedToBuild ->
            logI "Need to build" >> newBuild conf directives.language

  logI "Build complete, executing script"
  runScript opts conf

main :: IO ()
main = do
  cmd <- getCommand
  case cmd of
    Clean opts -> cleanCache opts
    Run opts -> runMagix opts
