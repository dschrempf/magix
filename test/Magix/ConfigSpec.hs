-- |
-- Module      :  Magix.ConfigSpec
-- Description :  Unit tests of the configuration
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 17 09:24:42 2026.
module Magix.ConfigSpec
  ( spec,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Text (Text)
import Magix.Config (Config (..), getConfig)
import Magix.Env (MagixEnv (..), NixEnv (..))
import Magix.Hash (getSlotHash)
import Magix.Options (Options (..), Rebuild (..), Verbosity (..))
import Magix.Paths (getSlotDir)
import Magix.Tools (getRandomTempDir)
import System.Directory
  ( canonicalizePath,
    copyFile,
    createDirectory,
    removeFile,
    withCurrentDirectory,
  )
import System.FilePath ((</>))
import System.Posix (createSymbolicLink)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)

-- | A script, a copy of it at another canonical path, and symbolic links to
-- the script, named other than the script.
data Fixture = Fixture
  { cache :: !FilePath,
    script :: !FilePath,
    copy :: !FilePath,
    linkDir :: !FilePath,
    link :: !FilePath,
    otherLink :: !FilePath
  }

contents :: ByteString
contents = "#!/usr/bin/env magix\n#!magix bash\n\necho Hello\n"

ref :: Text
ref = "github:NixOS/nixpkgs/nixos-unstable"

getFixture :: IO Fixture
getFixture = do
  -- Canonical, so that the expected paths match the working directory.
  tmp <- getRandomTempDir >>= canonicalizePath
  let dir name = tmp </> name
      fx =
        Fixture
          { cache = dir "cache",
            script = dir "old" </> "script",
            copy = dir "new" </> "script",
            linkDir = dir "bin",
            link = dir "bin" </> "run",
            otherLink = dir "other" </> "run"
          }
  mapM_ (createDirectory . dir) ["old", "new", "bin", "other"]
  BS.writeFile fx.script contents
  copyFile fx.script fx.copy
  createSymbolicLink fx.script fx.link
  createSymbolicLink fx.script fx.otherLink
  pure fx

-- | Read neither @NIX_PATH@ nor the XDG cache directory.
getConfigWith :: Fixture -> Text -> ByteString -> FilePath -> IO Config
getConfigWith fx r xs p =
  getConfig
    Options
      { verbosity = Info,
        forceBuild = ReuseBuildIfAvailable,
        cachePath = Just fx.cache,
        buildMode = Nothing,
        magixEnv = MagixEnv Nothing,
        nixEnv = NixEnv Nothing,
        scriptPath = p,
        scriptArgs = []
      }
    xs
    (Just r)

getConfigOf :: Fixture -> FilePath -> IO Config
getConfigOf fx = getConfigWith fx ref contents

-- | Invoke the script by the relative path of its link. Specs run sequentially
-- (none uses `parallel`), so changing the working directory is safe.
getConfigRelative :: Fixture -> IO Config
getConfigRelative fx = withCurrentDirectory fx.linkDir $ getConfigOf fx "run"

-- | Point the link to the copy, as a deploy does.
deploy :: Fixture -> IO ()
deploy fx = removeFile fx.link >> createSymbolicLink fx.copy fx.link

spec :: Spec
spec = do
  describe "getConfig" $ do
    it "keeps symbolic links in the invoked path and resolves them in the script path" $ do
      fx <- getFixture
      c <- getConfigRelative fx
      c.invokedPath `shouldBe` fx.link
      c.scriptPath `shouldBe` fx.script

    it "names the slot and the script by the invoked path" $ do
      fx <- getFixture
      c <- getConfigRelative fx
      c.scriptName `shouldBe` "run"
      c.slotDir `shouldBe` getSlotDir fx.cache (getSlotHash fx.link) "run"

    it "separates the slots of a script invoked by different paths" $ do
      fx <- getFixture
      c <- getConfigRelative fx
      c' <- getConfigOf fx fx.otherLink
      c'.scriptPath `shouldBe` c.scriptPath
      c'.slotDir `shouldNotBe` c.slotDir

    it "keeps the slot and the hash when a deploy moves the script" $ do
      fx <- getFixture
      c <- getConfigRelative fx
      deploy fx
      c' <- getConfigRelative fx
      c'.scriptPath `shouldBe` fx.copy
      c'.slotDir `shouldBe` c.slotDir
      c'.magixHash `shouldBe` c.magixHash

    it "hashes the same contents at another canonical path alike" $ do
      fx <- getFixture
      c <- getConfigOf fx fx.script
      c' <- getConfigOf fx fx.copy
      c'.magixHash `shouldBe` c.magixHash

    it "hashes other contents differently" $ do
      fx <- getFixture
      c <- getConfigRelative fx
      c' <- withCurrentDirectory fx.linkDir $ getConfigWith fx ref (contents <> "\n") "run"
      c'.magixHash `shouldNotBe` c.magixHash

    it "hashes another build mode differently" $ do
      fx <- getFixture
      c <- getConfigRelative fx
      c' <- withCurrentDirectory fx.linkDir $ getConfigWith fx "nixpkgs" contents "run"
      c'.magixHash `shouldNotBe` c.magixHash
