-- |
-- Module      :  Magix.Tools
-- Description :  Helpers shared across tests
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Oct 21 21:56:26 2024.
module Magix.Tools
  ( withTempDir,
    withFakeConfig,
    withFakeFlakeConfig,
    setFakeHash,
    parseS,
    parseF,
    parseFContaining,
    testExpression,
    testFlakeExpression,
  )
where

import Data.Bifunctor (Bifunctor (first))
import Data.ByteString (ByteString)
import Data.Either (isLeft)
import Data.List qualified as L
import Data.Text (Text, isInfixOf, unwords)
import Magix.BuildMode (BuildMode (..))
import Magix.Config (Config (..))
import Magix.Expression (getFlakeWrapper, getNixExpression, getReplacements, getTemplate)
import Magix.Hash (getSlotHash)
import Magix.Language.Directives (LanguageDirectives, getLanguage)
import Magix.Paths
  ( getBuildDir,
    getBuildExprPath,
    getCacheLockPath,
    getInvokedPathLink,
    getLockPath,
    getResultLinkPath,
    getSlotDir,
  )
import System.Directory (canonicalizePath, createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Expectation, HasCallStack, Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec
  ( Parsec,
    ShowErrorComponent,
    TraversableStream,
    VisualStream,
    errorBundlePretty,
    parse,
  )
import Prelude hiding (unwords)

-- | Run the action in a fresh temporary directory, which is removed afterwards.
-- The path is canonical, so that resolved paths match it.
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir action = withSystemTempDirectory "magix" $ \dir -> canonicalizePath dir >>= action

-- | A configuration of a script in a fresh temporary directory, with its cache
-- and slot directories; the script does not exist.
withFakeConfig :: (Config -> IO a) -> IO a
withFakeConfig action = withTempDir $ \tmp -> do
  let invokedPath = tmp </> "fakeScriptPath"
      scriptName = "fakeScriptName"
      cacheDir = tmp </> "fakeCacheDir"
      slotDir = getSlotDir cacheDir (getSlotHash invokedPath) scriptName
  createDirectoryIfMissing True slotDir
  action $
    setFakeHash "fakeHash" $
      Config
        { invokedPath = invokedPath,
          scriptPath = invokedPath,
          scriptName = scriptName,
          buildMode = ChannelBuild (tmp </> "fakeNixpkgsPath"),
          magixHash = "",
          cacheDir = cacheDir,
          cacheLockPath = getCacheLockPath cacheDir,
          slotDir = slotDir,
          lockPath = getLockPath slotDir,
          invokedPathLink = getInvokedPathLink slotDir,
          buildDir = "",
          buildExprPath = "",
          resultLinkPath = ""
        }

withFakeFlakeConfig :: Text -> (Config -> IO a) -> IO a
withFakeFlakeConfig ref action = withFakeConfig $ \c -> action c {buildMode = FlakeBuild ref}

-- | Set the hash of the contents, and the paths of the build named by it.
setFakeHash :: ByteString -> Config -> Config
setFakeHash hsh c =
  c
    { magixHash = hsh,
      buildDir = buildDir,
      buildExprPath = getBuildExprPath buildDir,
      resultLinkPath = getResultLinkPath c.slotDir hsh
    }
  where
    buildDir = getBuildDir c.slotDir hsh

parseS ::
  (HasCallStack, Show a, Eq a) =>
  (VisualStream s, TraversableStream s, ShowErrorComponent e) =>
  Parsec e s a ->
  s ->
  a ->
  Expectation
parseS p x e = first errorBundlePretty (parse p "" x) `shouldBe` Right e

parseF ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e, Show b) =>
  Parsec e s b ->
  s ->
  Expectation
parseF p x = first errorBundlePretty (parse p "" x) `shouldSatisfy` isLeft

-- | The parse fails with an error message containing the given text.
parseFContaining ::
  (HasCallStack, VisualStream s, TraversableStream s, ShowErrorComponent e, Show b) =>
  Parsec e s b ->
  s ->
  String ->
  Expectation
parseFContaining p x m =
  first errorBundlePretty (parse p "" x) `shouldSatisfy` either (m `L.isInfixOf`) (const False)

allReplacementsUsed :: Text -> [(Text, Text)] -> Bool
allReplacementsUsed x = all (\(r, _) -> r `isInfixOf` x)

doesNotContainTemplates :: Text -> Bool
doesNotContainTemplates = not . isInfixOf "__"

containsSpaceSeparatedValues :: [Text] -> Text -> Bool
containsSpaceSeparatedValues xs = isInfixOf (unwords xs)

testFlakeExpression :: Text -> LanguageDirectives -> [[Text]] -> Spec
testFlakeExpression ref directives values = do
  describe (withName "getReplacements (Flake)") $ do
    it "all replacements should be used as placeholders in the language template" $
      withFakeFlakeConfig ref $ \config -> do
        templ <- getTemplate language
        allReplacementsUsed templ (getReplacements config.scriptName directives) `shouldBe` True

  describe (withName "getNixExpression (Flake)") $ do
    it "all placeholders in language template should be replaced" $
      withFakeFlakeConfig ref $ \config -> do
        expr <- getNixExpression config.scriptName directives
        expr `shouldSatisfy` doesNotContainTemplates

  describe (withName "getFlakeWrapper (Flake)") $ do
    it "Flake wrapper contains the Nixpkgs reference" $ do
      wrapper <- getFlakeWrapper ref
      wrapper `shouldSatisfy` isInfixOf ref

    it "Flake wrapper contains no unreplaced placeholders" $ do
      wrapper <- getFlakeWrapper ref
      wrapper `shouldSatisfy` doesNotContainTemplates

  describe (withName "getNixExpression (Flake)") $ do
    it "works correctly for some sample data" $
      withFakeFlakeConfig ref $ \config -> do
        expr <- getNixExpression config.scriptName directives
        sequence_ [expr `shouldSatisfy` containsSpaceSeparatedValues value | value <- values]
  where
    language = getLanguage directives
    withName xs = "[" <> show language <> "] " <> xs

testExpression :: LanguageDirectives -> [[Text]] -> Spec
testExpression directives values = do
  describe (withName "getReplacements") $ do
    it "all replacements should be used as placeholders in the templates" $
      withFakeConfig $ \config -> do
        templ <- getTemplate language
        allReplacementsUsed templ (getReplacements config.scriptName directives) `shouldBe` True

  describe (withName "getNixExpression") $ do
    it "all placeholders in templates should be replaced" $
      withFakeConfig $ \config -> do
        expr <- getNixExpression config.scriptName directives
        expr `shouldSatisfy` doesNotContainTemplates

  describe (withName "getNixExpression") $ do
    it "works correctly for some sample data" $
      withFakeConfig $ \config -> do
        expr <- getNixExpression config.scriptName directives
        sequence_ [expr `shouldSatisfy` containsSpaceSeparatedValues value | value <- values]
  where
    language = getLanguage directives
    withName xs = "[" <> show language <> "] " <> xs
