-- |
-- Module      :  Magix.OptionsSpec
-- Description :  Unit tests of the command line
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 17 16:00:00 2026.
module Magix.OptionsSpec
  ( spec,
  )
where

import Data.List (isInfixOf)
import Magix.BuildMode (BuildMode (..))
import Magix.Env (MagixEnv (..), NixEnv (..))
import Magix.Options
  ( CleanMode (..),
    CleanOptions (..),
    Command (..),
    Options (..),
    PrintOptions (..),
    RawCommand (..),
    RawOptions (..),
    Rebuild (..),
    Verbosity (..),
    rawCommandParser,
    resolveCommand,
  )
import Options.Applicative
  ( ParserResult (..),
    defaultPrefs,
    execParserPure,
    renderFailure,
  )
import System.Exit (ExitCode (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | Parse the arguments as production does; 'Left' holds the rendered failure
-- (help, version or error) and its exit code.
parse :: [String] -> Either (String, ExitCode) RawCommand
parse args = case execParserPure defaultPrefs rawCommandParser args of
  Success r -> Right r
  Failure f -> Left $ renderFailure f "magix"
  CompletionInvoked _ -> error "parse: completion invoked"

-- | Parse the arguments, and resolve them against the environment.
resolve :: MagixEnv (Maybe String) -> NixEnv (Maybe String) -> [String] -> Either String Command
resolve magixEnv nixEnv args = case parse args of
  Left (msg, _) -> Left msg
  Right raw -> resolveCommand raw magixEnv nixEnv

noMagixEnv :: MagixEnv (Maybe String)
noMagixEnv = MagixEnv Nothing

noNixEnv :: NixEnv (Maybe String)
noNixEnv = NixEnv Nothing Nothing Nothing Nothing

-- | Raw options of @magix script@.
defaultRaw :: RawOptions
defaultRaw =
  RawOptions
    { verbosity = Info,
      forceBuild = ReuseBuildIfAvailable,
      cachePath = Nothing,
      nixpkgsPath = Nothing,
      nixpkgsRef = Nothing,
      scriptPath = "script",
      scriptArgs = []
    }

-- | A field of the raw options the arguments parse to.
rawField :: (RawOptions -> a) -> [String] -> Either String a
rawField f args = case parse args of
  Right (RawRun r) -> Right $ f r
  Right c -> Left $ "not a run command: " <> show c
  Left (msg, _) -> Left msg

-- | The script and its arguments.
script :: RawOptions -> (FilePath, [String])
script r = (r.scriptPath, r.scriptArgs)

-- | The build mode @magix@ chooses for the arguments and @MAGIX_NIXPKGS_REF@.
buildModeOf :: Maybe String -> [String] -> Either String (Maybe BuildMode)
buildModeOf ref args = case resolve (MagixEnv ref) noNixEnv args of
  Right (Run o) -> Right o.buildMode
  Right c -> Left $ "not a run command: " <> show c
  Left e -> Left e

exclusive :: String
exclusive = "--nixpkgs-path and --nixpkgs-ref are mutually exclusive"

clean :: Verbosity -> Maybe FilePath -> CleanMode -> Either a RawCommand
clean v c m = Right $ RawClean CleanOptions {verbosity = v, cachePath = c, cleanMode = m}

spec :: Spec
spec = do
  describe "rawCommandParser" $ do
    it "defaults every flag" $
      parse ["script"] `shouldBe` Right (RawRun defaultRaw)

    it "parses --verbose" $ do
      rawField (.verbosity) ["-v", "script"] `shouldBe` Right Debug
      rawField (.verbosity) ["--verbose", "script"] `shouldBe` Right Debug

    it "parses --force-build" $ do
      rawField (.forceBuild) ["-f", "script"] `shouldBe` Right ForceBuild
      rawField (.forceBuild) ["--force-build", "script"] `shouldBe` Right ForceBuild

    it "parses --cache-path" $ do
      rawField (.cachePath) ["-c", "/cache", "script"] `shouldBe` Right (Just "/cache")
      rawField (.cachePath) ["--cache-path", "/cache", "script"] `shouldBe` Right (Just "/cache")

    it "parses --nixpkgs-path" $ do
      rawField (.nixpkgsPath) ["-n", "/nixpkgs", "script"] `shouldBe` Right (Just "/nixpkgs")
      rawField (.nixpkgsPath) ["--nixpkgs-path", "/nixpkgs", "script"] `shouldBe` Right (Just "/nixpkgs")

    it "parses --nixpkgs-ref" $ do
      rawField (.nixpkgsRef) ["-r", "nixpkgs", "script"] `shouldBe` Right (Just "nixpkgs")
      rawField (.nixpkgsRef) ["--nixpkgs-ref", "nixpkgs", "script"] `shouldBe` Right (Just "nixpkgs")

    it "parses all flags together" $
      parse ["-v", "-f", "-c", "/cache", "-n", "/nixpkgs", "-r", "nixpkgs", "script", "a"]
        `shouldBe` Right
          ( RawRun
              RawOptions
                { verbosity = Debug,
                  forceBuild = ForceBuild,
                  cachePath = Just "/cache",
                  nixpkgsPath = Just "/nixpkgs",
                  nixpkgsRef = Just "nixpkgs",
                  scriptPath = "script",
                  scriptArgs = ["a"]
                }
          )

    it "parses the shared flags after an option of a run" $
      rawField (\r -> (r.verbosity, r.cachePath, r.forceBuild)) ["-f", "-c", "/cache", "-v", "script"]
        `shouldBe` Right (Debug, Just "/cache", ForceBuild)

    it "passes the arguments after the script to the script" $
      rawField script ["script", "a", "b"] `shouldBe` Right ("script", ["a", "b"])

    it "passes options after the script to the script, -h and -v included" $ do
      let args = ["-h", "-v", "--verbose", "-f", "-c", "/cache", "--version", "--stale", "--", "a"]
      rawField script ("script" : args) `shouldBe` Right ("script", args)
      rawField (\r -> (r.verbosity, script r)) ("-v" : "script" : args)
        `shouldBe` Right (Debug, ("script", args))

    it "passes clean-magix-cache after the script to the script" $
      rawField script ["script", "clean-magix-cache", "--stale"]
        `shouldBe` Right ("script", ["clean-magix-cache", "--stale"])

    it "fails without a script" $
      parse [] `shouldSatisfy` failsWith (ExitFailure 1) "Missing: (COMMAND | SCRIPT_FILE_PATH)"

    it "fails on an unknown option before the script" $
      parse ["--unknown", "script"] `shouldSatisfy` failsWith (ExitFailure 1) "Invalid option"

    it "shows help on -h before the script" $
      parse ["-h"] `shouldSatisfy` failsWith ExitSuccess "MAGIX_NIXPKGS_REF"

    it "shows the version on --version before the script" $
      parse ["--version"] `shouldSatisfy` failsWith ExitSuccess "Magix version"

    describe "clean-magix-cache" $ do
      it "removes all by default" $
        parse ["clean-magix-cache"] `shouldBe` clean Info Nothing RemoveAll

      it "parses --stale" $
        parse ["clean-magix-cache", "--stale"] `shouldBe` clean Info Nothing RemoveStale

      it "parses --verbose and --cache-path" $ do
        let r = clean Debug (Just "/cache") RemoveAll
        parse ["clean-magix-cache", "-v", "-c", "/cache"] `shouldBe` r
        parse ["clean-magix-cache", "--verbose", "--cache-path", "/cache"] `shouldBe` r

      it "parses the flags in any order" $
        parse ["clean-magix-cache", "--stale", "-c", "/cache", "-v"]
          `shouldBe` clean Debug (Just "/cache") RemoveStale

      it "parses --verbose and --cache-path before the command" $ do
        let r = clean Debug (Just "/cache") RemoveStale
        parse ["-v", "-c", "/cache", "clean-magix-cache", "--stale"] `shouldBe` r
        parse ["--verbose", "--cache-path", "/cache", "clean-magix-cache", "--stale"] `shouldBe` r
        parse ["-c", "/cache", "-v", "clean-magix-cache", "--stale"] `shouldBe` r

      it "parses --verbose and --cache-path on either side of the command" $ do
        let r = clean Debug (Just "/cache") RemoveAll
        parse ["-v", "clean-magix-cache", "-c", "/cache"] `shouldBe` r
        parse ["-c", "/cache", "clean-magix-cache", "-v"] `shouldBe` r
        parse ["-v", "clean-magix-cache", "-v", "-c", "/cache"] `shouldBe` r

      it "prefers --cache-path after the command to --cache-path before it" $
        parse ["-c", "/before", "clean-magix-cache", "-c", "/after"]
          `shouldBe` clean Info (Just "/after") RemoveAll

      it "runs a script of that name after an option of a run" $ do
        rawField (\r -> (r.forceBuild, script r)) ["-f", "clean-magix-cache", "--stale"]
          `shouldBe` Right (ForceBuild, ("clean-magix-cache", ["--stale"]))
        rawField (\r -> (r.nixpkgsPath, script r)) ["-n", "/nixpkgs", "-v", "clean-magix-cache"]
          `shouldBe` Right (Just "/nixpkgs", ("clean-magix-cache", []))
        rawField (\r -> (r.nixpkgsRef, script r)) ["-v", "-r", "nixpkgs", "clean-magix-cache"]
          `shouldBe` Right (Just "nixpkgs", ("clean-magix-cache", []))

      it "runs a script of that name given as a path" $ do
        rawField script ["./clean-magix-cache", "--stale"]
          `shouldBe` Right ("./clean-magix-cache", ["--stale"])
        rawField (\r -> (r.verbosity, script r)) ["-v", "./clean-magix-cache"]
          `shouldBe` Right (Debug, ("./clean-magix-cache", []))

      it "rejects --stale outside clean-magix-cache" $
        parse ["--stale", "script"] `shouldSatisfy` failsWith (ExitFailure 1) "Invalid option"

      it "rejects the options of a run" $
        parse ["clean-magix-cache", "-f"] `shouldSatisfy` failsWith (ExitFailure 1) "Invalid option"

    describe "print-magix-expression" $ do
      it "parses the script and --verbose" $ do
        let r = Right $ RawPrint PrintOptions {verbosity = Debug, scriptPath = "script"}
        parse ["print-magix-expression", "-v", "script"] `shouldBe` r
        parse ["print-magix-expression", "script", "--verbose"] `shouldBe` r

      it "parses --verbose before the command, and ignores --cache-path there" $
        parse ["-v", "-c", "/cache", "print-magix-expression", "script"]
          `shouldBe` Right (RawPrint PrintOptions {verbosity = Debug, scriptPath = "script"})

      it "rejects the options of a run" $
        parse ["print-magix-expression", "-f", "script"] `shouldSatisfy` failsWith (ExitFailure 1) "Invalid option"

  describe "resolveCommand" $ do
    it "passes print-magix-expression through, also without NIX_PATH" $
      resolve noMagixEnv noNixEnv ["print-magix-expression", "script"]
        `shouldBe` Right (Print PrintOptions {verbosity = Info, scriptPath = "script"})

    it "passes clean-magix-cache through" $
      resolve noMagixEnv noNixEnv ["clean-magix-cache", "-v", "--stale"]
        `shouldBe` Right (Clean CleanOptions {verbosity = Debug, cachePath = Nothing, cleanMode = RemoveStale})

    it "passes clean-magix-cache through with the flags before it" $
      resolve noMagixEnv noNixEnv ["-v", "-c", "/cache", "clean-magix-cache", "--stale"]
        `shouldBe` Right (Clean CleanOptions {verbosity = Debug, cachePath = Just "/cache", cleanMode = RemoveStale})

    it "passes the flags and the environment into the options" $ do
      let magixEnv = MagixEnv (Just "github:NixOS/nixpkgs/nixos-unstable")
          nixEnv = NixEnv (Just "nixpkgs=/n") (Just "/nch") (Just "/xdg") (Just "/ncd")
      resolve magixEnv nixEnv ["-v", "-f", "-c", "/cache", "script", "a", "-h"]
        `shouldBe` Right
          ( Run
              Options
                { verbosity = Debug,
                  forceBuild = ForceBuild,
                  cachePath = Just "/cache",
                  buildMode = Just (FlakeBuild "github:NixOS/nixpkgs/nixos-unstable"),
                  magixEnv = magixEnv,
                  nixEnv = nixEnv,
                  scriptPath = "script",
                  scriptArgs = ["a", "-h"]
                }
          )

    it "leaves the build mode open without --nixpkgs-path and a reference" $
      buildModeOf Nothing ["script"] `shouldBe` Right Nothing

    it "builds against --nixpkgs-path" $
      buildModeOf Nothing ["-n", "/nixpkgs", "script"] `shouldBe` Right (Just (ChannelBuild "/nixpkgs"))

    it "builds against --nixpkgs-ref" $
      buildModeOf Nothing ["-r", "nixpkgs", "script"] `shouldBe` Right (Just (FlakeBuild "nixpkgs"))

    it "builds against MAGIX_NIXPKGS_REF alone" $
      buildModeOf (Just "env") ["script"] `shouldBe` Right (Just (FlakeBuild "env"))

    it "prefers --nixpkgs-ref to MAGIX_NIXPKGS_REF" $
      buildModeOf (Just "env") ["-r", "flag", "script"] `shouldBe` Right (Just (FlakeBuild "flag"))

    it "rejects --nixpkgs-path with --nixpkgs-ref" $
      buildModeOf Nothing ["-n", "/nixpkgs", "-r", "nixpkgs", "script"] `shouldBe` Left exclusive

    it "rejects --nixpkgs-path with MAGIX_NIXPKGS_REF" $
      buildModeOf (Just "env") ["-n", "/nixpkgs", "script"] `shouldBe` Left exclusive
  where
    failsWith code needle r = case r of
      Left (msg, c) -> c == code && needle `isInfixOf` msg
      Right _ -> False
