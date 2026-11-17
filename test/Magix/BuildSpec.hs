-- |
-- Module      :  Magix.BuildSpec
-- Description :  Unit tests of builder
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 24 16:57:25 2024.
module Magix.BuildSpec
  ( spec,
  )
where

import Magix.Build (BuildStatus (..), getBuildStatus)
import Magix.Config (Config (..))
import Magix.Tools (withFakeConfig)
import System.Posix (createSymbolicLink)
import Test.Hspec (Spec, describe, it, shouldReturn)

spec :: Spec
spec = do
  describe "getBuildStatus" $ do
    it "detects non-existing builds" $ do
      withFakeConfig $ \c -> getBuildStatus c `shouldReturn` NeedToBuild

    it "detects dangling symbolic links to build result" $ do
      withFakeConfig $ \c -> do
        let rp = c.resultLinkPath
        createSymbolicLink "nonexistent" rp
        getBuildStatus c `shouldReturn` NeedToBuild
