-- |
-- Module      :  Magix.LockedFlakeSpec
-- Description :  Lock flake references with Nix
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Nov 18 10:00:00 2026.
module Magix.LockedFlakeSpec
  ( spec,
  )
where

import Data.Text (Text, isInfixOf, pack)
import Magix.Env (NixEnv (..))
import Magix.LockedFlake (LockedFlake (..), lockFlakeRef)
import System.Directory (createDirectory, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe, shouldReturn, shouldSatisfy)

-- | A flake without lock file in @<dir>/flake@, with the input @dep@, the
-- directory @<dir>/dep@.
withFlake :: (FilePath -> Text -> IO a) -> IO a
withFlake action = withSystemTempDirectory "magix-flake" $ \dir -> do
  let flake = dir </> "flake"
      dep = dir </> "dep"
  mapM_ createDirectory [flake, dep]
  writeFile (dep </> "lib.nix") "1\n"
  writeFile (flake </> "flake.nix") $
    "{ inputs.dep = { url = \"path:" <> dep <> "\"; flake = false; }; outputs = _: { }; }\n"
  action dir $ pack $ "path:" <> flake

lock :: Text -> IO LockedFlake
lock = lockFlakeRef $ NixEnv Nothing Nothing Nothing Nothing

spec :: Spec
spec = describe "lockFlakeRef" $ do
  it "locks a path flake to a reference fixing its contents, writing no lock file" $
    withFlake $ \dir r -> do
      f <- lock r
      f.ref `shouldSatisfy` isInfixOf "narHash="
      doesFileExist (dir </> "flake" </> "flake.lock") `shouldReturn` False

  it "locks an edited path flake to other contents" $
    withFlake $ \dir r -> do
      f <- lock r
      appendFile (dir </> "flake" </> "flake.nix") "# edited\n"
      f' <- lock r
      f'.narHashes `shouldNotBe` f.narHashes

  it "locks a path flake whose unlocked input changed to other contents" $
    withFlake $ \dir r -> do
      f <- lock r
      writeFile (dir </> "dep" </> "lib.nix") "2\n"
      f' <- lock r
      f'.ref `shouldBe` f.ref
      f'.narHashes `shouldNotBe` f.narHashes
