-- |
-- Module      :  Magix.MainSpec
-- Description :  Unit tests of building or reusing scripts, and cleaning the cache
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 17 11:15:00 2026.
module Magix.MainSpec
  ( spec,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as B
import Control.Exception (bracket)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.List (isPrefixOf, isSuffixOf, sort)
import Data.Text qualified as T
import Data.Text.Encoding.Error (UnicodeException)
import Magix.BuildMode (BuildMode (..))
import Magix.Directives (DirectivesParseError)
import Magix.Env (MagixEnv (..), NixEnv (..))
import Magix.Hash (getSlotHash)
import Magix.Main (cleanCache, getScriptExpression, runMagix)
import Magix.Options (CleanMode (..), CleanOptions (..), Options (..), PrintOptions (PrintOptions), Rebuild (..), Verbosity (..))
import Magix.Paths (getSlotDir)
import Magix.StubNix (Call (..), StubNix, getStoreDir, readCalls, withStubNix)
import Magix.Tools (withTempDir)
import System.Directory (createDirectory, doesDirectoryExist, doesFileExist, listDirectory, removeFile)
import System.Exit (ExitCode (..))
import System.FilePath (takeDirectory, takeFileName, (</>))
import System.Log.Formatter (nullFormatter)
import System.Log.Handler.Simple (GenericHandler (..))
import System.Log.Logger (Logger, Priority (..), getLogger, removeHandler, rootLoggerName, setHandlers, setLevel, updateGlobalLogger)
import System.Posix.Env (getEnv, setEnv, unsetEnv)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldReturn, shouldSatisfy, shouldThrow)

scriptName :: String
scriptName = "script"

script :: ByteString
script = "#!/usr/bin/env magix\n#!magix bash\necho Hello\n"

-- | A logger dropping the errors of failing runs.
getQuietLogger :: IO Logger
getQuietLogger = setLevel EMERGENCY <$> getLogger "Magix.MainSpec"

data Env = Env
  { stub :: !StubNix,
    cacheDir :: !FilePath,
    -- | Options running the script @<tmp>/script@, which does not exist.
    options :: !Options
  }

-- | Run the action with stubs of Nix, a cache, and a Nixpkgs path in a
-- temporary directory.
withEnv :: (FilePath -> Env -> IO a) -> IO a
withEnv action = withTempDir $ \tmp -> withStubNix scriptName $ \stub -> do
  let cacheDir = tmp </> "cache"
      nixpkgsDir = tmp </> "nixpkgs"
  mapM_ createDirectory [cacheDir, nixpkgsDir]
  action tmp $
    Env
      { stub = stub,
        cacheDir = cacheDir,
        options =
          Options
            { verbosity = Info,
              forceBuild = ReuseBuildIfAvailable,
              cachePath = Just cacheDir,
              buildMode = Just (ChannelBuild nixpkgsDir),
              magixEnv = MagixEnv Nothing,
              nixEnv = NixEnv Nothing Nothing Nothing Nothing,
              scriptPath = tmp </> scriptName,
              scriptArgs = []
            }
      }

-- | Run the script of the options with the given contents.
withScript :: ByteString -> (Env -> IO a) -> IO a
withScript contents action = withEnv $ \_ e -> do
  B.writeFile e.options.scriptPath contents
  action e

run :: Options -> IO FilePath
run opts = getQuietLogger >>= (`runMagix` opts)

-- | Run the action with a logger at the priority, and return the messages it
-- logged.
getLogged :: Priority -> (Logger -> IO ()) -> IO [String]
getLogged prio action = do
  ref <- newIORef []
  let handler =
        GenericHandler
          { priority = prio,
            formatter = nullFormatter,
            privData = (),
            writeFunc = \() msg -> modifyIORef' ref (msg :),
            closeFunc = pure
          }
  -- The handler of the root logger would print the messages as well.
  updateGlobalLogger rootLoggerName removeHandler
  logger <- setHandlers [handler] . setLevel prio <$> getLogger "Magix.MainSpec.Logged"
  action logger
  reverse <$> readIORef ref

-- | Run, and return the messages logged at 'INFO', the default of magix.
runLoggingInfo :: Options -> IO [String]
runLoggingInfo opts = getLogged INFO $ \logger -> () <$ runMagix logger opts

-- | Run, expect the run to exit with failure, and return the errors it logged.
runLoggingErrors :: Options -> IO [String]
runLoggingErrors opts =
  getLogged ERROR $ \logger -> runMagix logger opts `shouldThrow` (== ExitFailure 1)

-- | Whether the errors are one, naming the script of the options unreadable
-- for the reason.
isUnreadableError :: Options -> String -> [String] -> Bool
isUnreadableError opts reason = \case
  [err] ->
    ("Could not read file: " <> opts.scriptPath <> ": ") `isPrefixOf` err
      && reason `isSuffixOf` err
  _otherwise -> False

clean :: FilePath -> CleanMode -> IO ()
clean cacheDir mode = do
  logger <- getQuietLogger
  cleanCache logger $ CleanOptions {verbosity = Info, cachePath = Just cacheDir, cleanMode = mode}

-- | Run the action with the environment variable set, or unset; restore it
-- afterwards. The environment is global to the process: run no tests in
-- parallel to this.
withEnvVar :: String -> Maybe String -> IO a -> IO a
withEnvVar name value action = bracket (getEnv name) set $ \_ -> set value >> action
  where
    set = maybe (unsetEnv name) (\v -> setEnv name v True)

getPrograms :: StubNix -> IO [String]
getPrograms stub = map (.program) <$> readCalls stub

-- | The entries of the cache directory but its lock.
getCacheEntries :: FilePath -> IO [FilePath]
getCacheEntries cacheDir = sort . filter (/= "lock") <$> listDirectory cacheDir

-- | The slot of the script invoked by the path.
getSlot :: Env -> FilePath -> FilePath
getSlot e path = getSlotDir e.cacheDir (getSlotHash path) scriptName

isExecutableOf :: StubNix -> FilePath -> Bool
isExecutableOf stub exe =
  takeFileName exe == scriptName
    && takeFileName (takeDirectory exe) == "bin"
    && takeDirectory (takeDirectory (takeDirectory exe)) == getStoreDir stub

spec :: Spec
spec = do
  describe "runMagix" $ do
    it "builds on the first run, and returns the executable of the build" $
      withScript script $ \e -> do
        exe <- run e.options
        getPrograms e.stub `shouldReturn` ["nix-build"]
        exe `shouldSatisfy` isExecutableOf e.stub
        doesFileExist exe `shouldReturn` True

    it "reuses the build on the second run" $
      withScript script $ \e -> do
        exe <- run e.options
        run e.options `shouldReturn` exe
        getPrograms e.stub `shouldReturn` ["nix-build"]

    it "rebuilds when forced, even when built" $
      withScript script $ \e -> do
        exe <- run e.options
        exe' <- run e.options {forceBuild = ForceBuild}
        getPrograms e.stub `shouldReturn` ["nix-build", "nix-build"]
        exe' `shouldNotBe` exe
        exe' `shouldSatisfy` isExecutableOf e.stub
        doesFileExist exe' `shouldReturn` True

    it "rebuilds an edited script, and keeps one build in the slot" $
      withScript script $ \e -> do
        _ <- run e.options
        B.writeFile e.options.scriptPath $ script <> "echo World\n"
        exe <- run e.options
        getPrograms e.stub `shouldReturn` ["nix-build", "nix-build"]
        doesFileExist exe `shouldReturn` True
        entries <- listDirectory $ getSlot e e.options.scriptPath
        length (filter ("-build" `isSuffixOf`) entries) `shouldBe` 1
        length (filter ("-result" `isSuffixOf`) entries) `shouldBe` 1

    it "builds a Flake for a nixpkgs directive" $
      withScript "#!/usr/bin/env magix\n#!nixpkgs nixpkgs\n#!magix bash\necho Hello\n" $ \e -> do
        exe <- run e.options
        map (\c -> (c.program, take 1 c.args)) <$> readCalls e.stub
          `shouldReturn` [("nix", ["build"])]
        exe `shouldSatisfy` isExecutableOf e.stub

    it "logs one line at INFO, naming the script, when it builds" $
      withScript script $ \e ->
        runLoggingInfo e.options `shouldReturn` ["Building script: " <> e.options.scriptPath]

    it "logs nothing at INFO when it reuses the build" $
      withScript script $ \e -> do
        _ <- run e.options
        runLoggingInfo e.options `shouldReturn` []

    it "throws, and calls no Nix, when the directives fail to parse" $
      withScript "#!/usr/bin/env magix\necho Hello\n" $ \e -> do
        run e.options `shouldThrow` (\(_ :: DirectivesParseError) -> True)
        readCalls e.stub `shouldReturn` []

    it "throws, and calls no Nix, when the script is no UTF-8" $
      withScript "#!/usr/bin/env magix\n#!magix bash\necho \xff\n" $ \e -> do
        run e.options `shouldThrow` (\(_ :: UnicodeException) -> True)
        readCalls e.stub `shouldReturn` []

    it "exits with failure, and logs why, when the script is missing" $
      withEnv $ \_ e -> do
        errs <- runLoggingErrors e.options
        errs `shouldSatisfy` isUnreadableError e.options "does not exist (No such file or directory)"
        readCalls e.stub `shouldReturn` []

    it "exits with failure, and logs why, when the script is a directory" $
      withEnv $ \_ e -> do
        createDirectory e.options.scriptPath
        errs <- runLoggingErrors e.options
        errs `shouldSatisfy` isUnreadableError e.options "inappropriate type (is a directory)"
        readCalls e.stub `shouldReturn` []

  describe "getScriptExpression" $ do
    it "returns the expression, builds nothing, and touches no cache, without NIX_PATH" $
      withScript script $ \e -> do
        let xdgCache = e.cacheDir </> "xdg"
        expr <-
          withEnvVar "NIX_PATH" Nothing $
            withEnvVar "XDG_CACHE_HOME" (Just xdgCache) $ do
              logger <- getQuietLogger
              getScriptExpression logger $ PrintOptions Info e.options.scriptPath
        expr `shouldSatisfy` T.isInfixOf "writeShellApplication"
        expr `shouldSatisfy` T.isInfixOf ("name = \"" <> T.pack scriptName <> "\"")
        expr `shouldSatisfy` T.isInfixOf "./script"
        readCalls e.stub `shouldReturn` []
        getCacheEntries e.cacheDir `shouldReturn` []
        doesDirectoryExist xdgCache `shouldReturn` False

    it "throws, and calls no Nix, when the directives fail to parse" $
      withScript "#!/usr/bin/env magix\necho Hello\n" $ \e -> do
        logger <- getQuietLogger
        getScriptExpression logger (PrintOptions Info e.options.scriptPath)
          `shouldThrow` (\(_ :: DirectivesParseError) -> True)
        readCalls e.stub `shouldReturn` []

  describe "cleanCache" $ do
    it "logs nothing at INFO" $
      withScript script $ \e -> do
        _ <- run e.options
        let cleanLogging mode =
              getLogged INFO $ \logger ->
                cleanCache logger $ CleanOptions {verbosity = Info, cachePath = Just e.cacheDir, cleanMode = mode}
        cleanLogging RemoveStale `shouldReturn` []
        cleanLogging RemoveAll `shouldReturn` []

    it "removes everything but the lock" $
      withScript script $ \e -> do
        _ <- run e.options
        getCacheEntries e.cacheDir >>= (`shouldNotBe` [])
        clean e.cacheDir RemoveAll
        getCacheEntries e.cacheDir `shouldReturn` []

    it "removes the slots of removed scripts, and keeps the others" $
      withEnv $ \tmp e -> do
        let kept = tmp </> "kept"
            gone = tmp </> "gone"
        mapM_ createDirectory [kept, gone]
        let keptPath = kept </> scriptName
            gonePath = gone </> scriptName
        mapM_ (`B.writeFile` script) [keptPath, gonePath]
        mapM_ (\p -> run e.options {scriptPath = p}) [keptPath, gonePath]
        getCacheEntries e.cacheDir
          `shouldReturn` sort (map (takeFileName . getSlot e) [keptPath, gonePath])
        removeFile gonePath
        clean e.cacheDir RemoveStale
        getCacheEntries e.cacheDir `shouldReturn` [takeFileName $ getSlot e keptPath]
