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

import Control.Exception (throwIO)
import Data.ByteString (readFile)
import Data.Text (unpack)
import Data.Text.Encoding (decodeUtf8')
import Magix.Build (BuildStatus (..), build, getBuildStatus, withBuildLock)
import Magix.Config (Config, getConfig)
import Magix.Directives (Directives, getDirectives)
import Magix.Expression (getNixExpression)
import Magix.Options (Options (..), Rebuild (..), Verbosity (..), getOptions)
import Magix.Run (run)
import System.IO (Handle, stderr)
import System.Log.Formatter (simpleLogFormatter)
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
    formatter = simpleLogFormatter "[$time $loggername $prio] $msg"

setupLogger :: Verbosity -> IO Logger
setupLogger v = do
  let prio = case v of
        Info -> INFO
        Debug -> DEBUG
  updateGlobalLogger rootLoggerName removeHandler
  stderrHandler <- withFormatter <$> streamHandler stderr prio
  logger <- setHandlers [stderrHandler] . setLevel prio <$> getLogger "Magix.Main"
  saveGlobalLogger logger
  pure logger

newBuild :: Config -> Directives -> IO ()
newBuild conf dirs = do
  logger <- getLogger "Magix.Main"
  let logD = logL logger DEBUG
      logI = logL logger INFO
  logD "Getting Nix expression"
  expr <- getNixExpression conf dirs
  logD $ "Nix expression is " <> unpack expr
  logI "Building Nix expression"
  build conf expr
  logD "Built Nix expression"

main :: IO ()
main = do
  opts <- getOptions
  logger <- setupLogger (verbosity opts)
  let logD = logL logger DEBUG
      logE = logL logger ERROR

  logD $ "Options are " <> show opts

  let p = scriptPath opts
  logD $ "Reading script at path " <> p
  bs <- readFile opts.scriptPath

  conf <- getConfig opts bs
  logD $ "Magix configuration is " <> show conf

  logD "Parsing directives"
  txt <- either throwIO pure $ decodeUtf8' bs
  dirs <- case getDirectives p txt of
    Left e -> logE "Failed parsing directives" >> throwIO e
    Right ds -> pure ds
  logD $ "Directives are " <> show dirs

  case forceBuild opts of
    ForceBuild -> do
      logD "Force build"
      withBuildLock conf $ newBuild conf dirs
    ReuseBuildIfAvailable -> do
      logD "Reuse build if available"
      logD "Checking build status"
      withBuildLock conf $ do
        buildStatus <- getBuildStatus conf
        case buildStatus of
          HasBeenBuilt -> logD "Script has already been built"
          NeedToBuild -> do
            logD "Need to build"
            newBuild conf dirs

  logD "Running"
  run opts conf
  logD "Done"
