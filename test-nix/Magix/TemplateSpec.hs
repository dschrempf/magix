-- |
-- Module      :  Magix.TemplateSpec
-- Description :  The generated Nix expressions evaluate
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 17 15:00:00 2026.
--
-- Needs @nix-instantiate@ and @nix@ on @PATH@, and @nixpkgs@ in @NIX_PATH@.
module Magix.TemplateSpec
  ( spec,
  )
where

import Control.Exception (throwIO)
import Control.Monad (forM_, unless)
import Data.Char (isSpace)
import Data.List (dropWhileEnd, isInfixOf)
import Data.Text (pack, unpack)
import Data.Text.IO qualified as T
import Magix.Build (nixBuildEnvironment)
import Magix.BuildMode (Nixpkgs (..))
import Magix.Config (Config (..))
import Magix.Directives (Directives (..), getDirectives)
import Magix.Expression (getFlakeWrapper, getNixExpression)
import Magix.Language.Directives (getLanguage)
import Magix.Language.Language (Language (..), getLanguageNameLowercase)
import Magix.LockedFlake (LockedFlake (..))
import System.Directory (copyFile, createDirectoryIfMissing)
import System.Environment (getEnvironment)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (CreateProcess (..), proc, readCreateProcessWithExitCode, readProcessWithExitCode)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)

-- | Run a command; fail with its standard error unless it succeeds.
succeeds :: FilePath -> [String] -> IO String
succeeds cmd args = do
  (code, out, err) <- readProcessWithExitCode cmd args ""
  unless (code == ExitSuccess) $
    expectationFailure $
      unwords (cmd : args) <> " failed with " <> show code <> ":\n" <> err
  pure out

succeeds_ :: FilePath -> [String] -> Expectation
succeeds_ cmd args = () <$ succeeds cmd args

nix :: [String] -> Expectation
nix args = succeeds_ "nix" $ ["--extra-experimental-features", "nix-command flakes"] <> args

-- | The Nixpkgs the tests build against: @<nixpkgs>@ from @NIX_PATH@.
getNixpkgs :: IO FilePath
getNixpkgs = dropWhileEnd isSpace <$> succeeds "nix-instantiate" ["--find-file", "nixpkgs"]

getMinimalScript :: Language -> FilePath
getMinimalScript language =
  "test-scripts" </> unpack (getLanguageNameLowercase language) </> "minimal"

-- | Only the script name enters the expression; the paths point into the build
-- directory.
getConfigIn :: FilePath -> Nixpkgs -> Config
getConfigIn dir mode =
  Config
    { invokedPath = dir </> "script",
      scriptPath = dir </> "script",
      scriptName = "minimal",
      nixpkgs = mode,
      magixHash = "",
      cacheDir = dir,
      cacheLockPath = dir </> "lock",
      slotDir = dir,
      lockPath = dir </> "lock",
      invokedPathLink = dir </> "path",
      buildDir = dir,
      buildExprPath = dir </> "default.nix",
      resultLinkPath = dir </> "result"
    }

-- | Write the build directory of the language's minimal test script, as
-- `Magix.Build.build` does, and run the action on it.
withBuildDir :: Language -> Nixpkgs -> (FilePath -> IO a) -> IO a
withBuildDir language = withScriptBuildDir language (getMinimalScript language)

withScriptBuildDir :: Language -> FilePath -> Nixpkgs -> (FilePath -> IO a) -> IO a
withScriptBuildDir language script mode action =
  withSystemTempDirectory "magix-template" $ \dir -> do
    directives <- T.readFile script >>= either throwIO pure . getDirectives script
    getLanguage directives.language `shouldBe` language
    copyFile script (dir </> "script")
    let cfg = getConfigIn dir mode
    getNixExpression cfg.scriptName directives.language >>= T.writeFile cfg.buildExprPath
    case mode of
      NixpkgsPath _ -> pure ()
      NixpkgsFlake flake -> getFlakeWrapper flake.ref >>= T.writeFile (dir </> "flake.nix")
    action dir

-- | Run the action on the build directory and the Nixpkgs path.
withChannelBuildDir :: Language -> (FilePath -> FilePath -> IO a) -> IO a
withChannelBuildDir language action = do
  nixpkgs <- getNixpkgs
  withBuildDir language (NixpkgsPath nixpkgs) (`action` nixpkgs)

-- | Instantiate the build directory against the Nixpkgs path, in the
-- environment `Magix.Build.build` gives @nix-build@ when magix runs with the
-- given variables set.
instantiate :: [(String, String)] -> FilePath -> FilePath -> IO (ExitCode, String, String)
instantiate vars dir nixpkgs = do
  magixEnv <- withVars <$> getEnvironment
  readCreateProcessWithExitCode
    (proc "nix-instantiate" ["-I", "nixpkgs=" <> nixpkgs, dir]) {env = Just $ nixBuildEnvironment magixEnv}
    ""
  where
    withVars e = filter ((`notElem` map fst vars) . fst) e <> vars

-- | Instantiate under a home whose Nixpkgs overlay fails when applied.
instantiateWithHomeOverlay :: FilePath -> FilePath -> IO (ExitCode, String, String)
instantiateWithHomeOverlay dir nixpkgs =
  withSystemTempDirectory "magix-home" $ \home -> do
    let nixpkgsConfigDir = home </> ".config" </> "nixpkgs"
    createDirectoryIfMissing True nixpkgsConfigDir
    writeFile (nixpkgsConfigDir </> "overlays.nix") "_: _: throw \"overlay applied\"\n"
    instantiate [("HOME", home)] dir nixpkgs

-- | The flake pins the Nixpkgs from @NIX_PATH@, so that it evaluates offline.
withFlakeBuildDir :: Language -> (FilePath -> IO a) -> IO a
withFlakeBuildDir language action = do
  nixpkgs <- getNixpkgs
  withBuildDir language (NixpkgsFlake $ LockedFlake (pack $ "path:" <> nixpkgs) []) action

spec :: Spec
spec = do
  -- The cache hash misses the variables of the environment.
  describe "nix-build" $
    it "ignores NIXPKGS_ALLOW_UNFREE" $
      withSystemTempDirectory "magix-script" $ \scriptDir -> do
        let script = scriptDir </> "unfree"
        writeFile script "#!/usr/bin/env magix\n#!magix bash\n#!packages unrar\n\nunrar\n"
        nixpkgs <- getNixpkgs
        withScriptBuildDir Bash script (NixpkgsPath nixpkgs) $ \dir -> do
          (code, _, err) <- instantiate [("NIXPKGS_ALLOW_UNFREE", "1")] dir nixpkgs
          code `shouldSatisfy` (/= ExitSuccess)
          err `shouldSatisfy` isInfixOf "unfree"

  forM_ [minBound .. maxBound :: Language] $ \language ->
    describe (show language) $ do
      describe "channel build" $ do
        it "default.nix parses" $
          withChannelBuildDir language $ \dir _ ->
            succeeds_ "nix-instantiate" ["--parse", dir </> "default.nix"]

        -- Like `Magix.Build.build`, which runs nix-build.
        it "default.nix instantiates against the hashed Nixpkgs" $
          withChannelBuildDir language $ \dir nixpkgs ->
            succeeds_ "nix-instantiate" ["-I", "nixpkgs=" <> nixpkgs, dir]

        -- The cache hash misses the overlays of the environment.
        it "default.nix ignores an overlay of the home" $
          withChannelBuildDir language $ \dir nixpkgs -> do
            (code, _, err) <- instantiateWithHomeOverlay dir nixpkgs
            unless (code == ExitSuccess) $ expectationFailure err

      describe "flake build" $ do
        it "flake.nix parses" $
          withFlakeBuildDir language $ \dir ->
            succeeds_ "nix-instantiate" ["--parse", dir </> "flake.nix"]

        it "the flake instantiates" $
          withFlakeBuildDir language $ \dir ->
            nix ["path-info", "--derivation", "--no-write-lock-file", "path:" <> dir]
