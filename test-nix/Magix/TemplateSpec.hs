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
import Data.List (dropWhileEnd)
import Data.Text (pack, unpack)
import Data.Text.IO qualified as T
import Magix.BuildMode (BuildMode (..))
import Magix.Config (Config (..))
import Magix.Directives (Directives (..), getDirectives)
import Magix.Expression (getFlakeWrapper, getNixExpression)
import Magix.Language.Directives (getLanguage)
import Magix.Language.Language (Language, getLanguageNameLowercase)
import System.Directory (copyFile)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (readProcessWithExitCode)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)

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
getConfigIn :: FilePath -> BuildMode -> Config
getConfigIn dir mode =
  Config
    { invokedPath = dir </> "script",
      scriptPath = dir </> "script",
      scriptName = "minimal",
      buildMode = mode,
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
withBuildDir :: Language -> BuildMode -> (FilePath -> IO a) -> IO a
withBuildDir language mode action =
  withSystemTempDirectory "magix-template" $ \dir -> do
    let script = getMinimalScript language
    directives <- T.readFile script >>= either throwIO pure . getDirectives script
    getLanguage directives.language `shouldBe` language
    copyFile script (dir </> "script")
    let cfg = getConfigIn dir mode
    getNixExpression cfg directives.language >>= T.writeFile cfg.buildExprPath
    case mode of
      ChannelBuild _ -> pure ()
      FlakeBuild ref -> getFlakeWrapper ref >>= T.writeFile (dir </> "flake.nix")
    action dir

-- | Run the action on the build directory and the Nixpkgs path.
withChannelBuildDir :: Language -> (FilePath -> FilePath -> IO a) -> IO a
withChannelBuildDir language action = do
  nixpkgs <- getNixpkgs
  withBuildDir language (ChannelBuild nixpkgs) (`action` nixpkgs)

-- | The flake pins the Nixpkgs from @NIX_PATH@, so that it evaluates offline.
withFlakeBuildDir :: Language -> (FilePath -> IO a) -> IO a
withFlakeBuildDir language action = do
  nixpkgs <- getNixpkgs
  withBuildDir language (FlakeBuild $ pack $ "path:" <> nixpkgs) action

spec :: Spec
spec =
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

      describe "flake build" $ do
        it "flake.nix parses" $
          withFlakeBuildDir language $ \dir ->
            succeeds_ "nix-instantiate" ["--parse", dir </> "flake.nix"]

        it "the flake instantiates" $
          withFlakeBuildDir language $ \dir ->
            nix ["path-info", "--derivation", "--no-write-lock-file", "path:" <> dir]
