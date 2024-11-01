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
  ( getRandomFakeConfig,
    parse',
    testExpression,
  )
where

import Control.Monad (replicateM)
import Data.Bifunctor (Bifunctor (..))
import Data.ByteString (ByteString, pack)
import Data.Text (Text, isInfixOf, unwords)
import Data.Word (Word8)
import Magix.Config (Config (..))
import Magix.Directives (Directives, getLanguageName)
import Magix.Expression (getNixExpression, getReplacements, getTemplate)
import System.Directory (createDirectory, getTemporaryDirectory)
import System.FilePath ((</>))
import System.Random.Stateful (randomIO, randomRIO)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec
  ( Parsec,
    ShowErrorComponent,
    TraversableStream,
    VisualStream,
    errorBundlePretty,
    parse,
  )
import Prelude hiding (unwords)

getRandomString :: IO String
getRandomString = replicateM 40 $ randomRIO ('a', 'z')

getRandomByteString :: IO ByteString
getRandomByteString = pack <$> replicateM 40 randomIO

getRandomFakeConfig :: IO Config
getRandomFakeConfig = do
  dir <- ("magix-" <>) <$> getRandomString
  tmp <- (</> dir) <$> getTemporaryDirectory
  createDirectory tmp
  hsh <- getRandomByteString
  pure $
    Config
      (tmp </> "fakeScriptPath")
      (tmp </> "fakeScriptName")
      (tmp </> "fakeNixpkgsPath")
      hsh
      (tmp </> "fakeCacheDir")
      (tmp </> "fakeLockPath")
      (tmp </> "fakeScriptLinkPath")
      (tmp </> "fakeBuildDir")
      (tmp </> "fakeBuildExprPath")
      (tmp </> "fakeResultDir")

parse' ::
  (VisualStream s, TraversableStream s, ShowErrorComponent e) => Parsec e s a -> s -> a
parse' p x = either (error . errorBundlePretty) id $ parse p "" x

allReplacementsUsed :: Text -> [(Text, Text)] -> Bool
allReplacementsUsed x = all (\(r, _) -> r `isInfixOf` x)

doesNotContainTemplates :: Text -> Bool
doesNotContainTemplates = not . isInfixOf "__"

containsSpaceSeparatedValues :: [Text] -> Text -> Bool
containsSpaceSeparatedValues xs = isInfixOf (unwords xs)

testExpression :: Directives -> [[Text]] -> Spec
testExpression directives values = do
  describe (withName "getReplacements") $ do
    it "all replacements should be used as placeholders in the templates" $ do
      config <- getRandomFakeConfig
      templ <- getTemplate languageName
      allReplacementsUsed templ (getReplacements config directives) `shouldBe` True

  describe (withName "getNixExpression") $ do
    it "all placeholders in templates should be replaced" $ do
      config <- getRandomFakeConfig
      expr <- getNixExpression config directives
      expr `shouldSatisfy` doesNotContainTemplates

  describe (withName "getNixExpression") $ do
    it "works correctly for some sample data" $ do
      config <- getRandomFakeConfig
      expr <- getNixExpression config directives
      sequence_ [expr `shouldSatisfy` containsSpaceSeparatedValues value | value <- values]
  where
    languageName = getLanguageName directives
    withName xs = "[" <> languageName <> "] " <> xs
