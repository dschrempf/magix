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

import Control.Monad (forM_)
import GHC.IO.Encoding (setLocaleEncoding)
import Magix
  ( CleanOptions (..),
    Command (..),
    Options (..),
    Verbosity (..),
    cleanCache,
    getCommand,
    runMagix,
  )
import System.Console.ANSI
  ( Color (Green, Red, White),
    ColorIntensity (Dull),
    ConsoleIntensity (BoldIntensity),
    ConsoleLayer (Foreground),
    SGR (Reset, SetColor, SetConsoleIntensity),
    setSGRCode,
  )
import System.Exit (exitSuccess)
import System.IO (Handle, hSetEncoding, mkTextEncoding, stderr, stdout)
import System.Log.Formatter (tfLogFormatter)
import System.Log.Handler (setFormatter)
import System.Log.Handler.Simple (GenericHandler, streamHandler)
import System.Log.Logger
  ( Logger,
    Priority (..),
    getLogger,
    removeHandler,
    rootLoggerName,
    saveGlobalLogger,
    setHandlers,
    setLevel,
    updateGlobalLogger,
  )
import System.Posix.Process (executeFile)

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

-- | Nix writes UTF-8 regardless of the locale; ROUNDTRIP passes invalid bytes
-- through instead of failing.
setUtf8Encoding :: IO ()
setUtf8Encoding = do
  enc <- mkTextEncoding "UTF-8//ROUNDTRIP"
  setLocaleEncoding enc
  -- The standard handles were created with the previous locale encoding.
  forM_ [stdout, stderr] (`hSetEncoding` enc)

main :: IO ()
main = do
  setUtf8Encoding
  cmd <- getCommand
  case cmd of
    Clean opts -> do
      logger <- setupLogger opts.verbosity
      cleanCache logger opts
      exitSuccess
    Run opts -> do
      logger <- setupLogger opts.verbosity
      exe <- runMagix logger opts
      executeFile exe False opts.scriptArgs Nothing
