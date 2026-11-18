-- |
-- Module      :  Magix.LockedFlake
-- Description :  Lock a Nixpkgs flake reference before hashing it
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Wed Nov 18 10:00:00 2026.
--
-- Nix resolves a flake reference again at every build, so its text alone does
-- not fix what a build uses: @nixpkgs@ follows the registry, a branch moves, a
-- directory changes. magix locks the reference first, and hashes the result.
module Magix.LockedFlake
  ( -- * Locked flakes
    LockedFlake (..),
    lockedFlakeBytes,

    -- * Kinds of references
    FlakeRefKind (..),
    classifyFlakeRef,

    -- * Locking
    lockFlakeRef,
    Metadata (..),
    parseMetadata,
  )
where

import Data.Aeson (Value, eitherDecodeStrict, withObject, (.:), (.:?))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Parser, parseEither)
import Data.ByteString (ByteString)
import Data.Char (isHexDigit)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Magix.Env (NixEnv)
import Magix.FlakeRegistry (PathFlake (..), parseFlakeId, renderPathFlake, resolveRegistryId)
import System.Exit (ExitCode (..), die)
import System.Process (readProcessWithExitCode)

-- | A flake reference, and the contents Nix resolves it to.
data LockedFlake = LockedFlake
  { -- | The reference to build from. Nix resolves the inputs of a flake
    -- without lock file anew at the build.
    ref :: !Text,
    -- | The NAR hashes of the flake and its inputs, reported by
    -- @nix flake metadata@; empty if @ref@ fixes the contents.
    narHashes :: ![Text]
  }
  deriving (Eq, Show)

-- | What the cache hash includes.
lockedFlakeBytes :: LockedFlake -> Text
lockedFlakeBytes f = T.intercalate "\n" $ f.ref : f.narHashes

-- | A flake reference, classified by its text.
data FlakeRefKind
  = -- | The text fixes the contents: a path in the store, a @rev@ or
    -- @narHash@ parameter, or a forge reference naming a commit.
    Fixed
  | -- | A bare flake id, with or without @flake:@, such as @nixpkgs@.
    FlakeId !Text
  | -- | Any other reference, such as a branch or a directory.
    Mutable
  deriving (Eq, Show)

storeDir :: Text
storeDir = "/nix/store/"

-- | Classify a flake reference by the URL forms Nix accepts. A reference
-- classified 'Mutable' asks Nix, so the classification errs that way.
classifyFlakeRef :: Text -> FlakeRefKind
classifyFlakeRef r
  | Right i <- parseFlakeId $ T.unpack $ dropPrefix "flake:" r = FlakeId i
  | storeDir `T.isPrefixOf` dropPrefix "path:" location = Fixed
  | any (`elem` ["rev", "narHash"]) parameterNames = Fixed
  | (scheme, path) <- T.breakOn ":" location,
    scheme `elem` ["github", "gitlab", "sourcehut"],
    _ : _ : rev : _ <- T.splitOn "/" (T.drop 1 path),
    isCommit rev =
      Fixed
  | otherwise = Mutable
  where
    (location, query) = T.breakOn "?" r
    parameterNames = map (T.takeWhile (/= '=')) $ T.splitOn "&" $ T.drop 1 query
    dropPrefix p x = fromMaybe x $ T.stripPrefix p x
    isCommit x = T.length x == 40 && T.all isHexDigit x

-- | Lock a flake reference.
--
-- - 'Fixed': as is.
-- - 'FlakeId': through the user and the system registry, if they resolve the
--   id to a path in the store, to the reference of the registry entry; Nix
--   resolves the id to the same one.
-- - Otherwise ask @nix flake metadata@, and build from the locked reference it
--   returns.
--
-- Die if Nix fails.
lockFlakeRef :: NixEnv (Maybe String) -> Text -> IO LockedFlake
lockFlakeRef env r = case classifyFlakeRef r of
  Fixed -> pure $ LockedFlake r []
  FlakeId i ->
    resolveRegistryId env i >>= \case
      (_, Right f) | storeDir `T.isPrefixOf` T.pack f.path -> pure $ LockedFlake (renderPathFlake f) []
      _ -> lockWithNix r
  Mutable -> lockWithNix r

-- | The arguments of @nix@ reporting the locked reference; writing no lock
-- file into the flake.
nixFlakeMetadataArgs :: Text -> [String]
nixFlakeMetadataArgs r = ["flake", "metadata", "--json", "--no-write-lock-file", T.unpack r]

lockWithNix :: Text -> IO LockedFlake
lockWithNix r = do
  (code, out, err) <- readProcessWithExitCode "nix" (nixFlakeMetadataArgs r) ""
  let failWith why = die $ "Cannot lock the Nixpkgs flake reference '" <> T.unpack r <> "': " <> why
  case code of
    ExitSuccess -> either failWith (pure . toLockedFlake) $ parseMetadata $ TE.encodeUtf8 $ T.pack out
    ExitFailure _ -> failWith $ "'nix " <> unwords (nixFlakeMetadataArgs r) <> "' failed:\n" <> err

-- | What magix reads of the output of @nix flake metadata --json@.
data Metadata = Metadata
  { -- | The locked reference.
    url :: !Text,
    narHash :: !Text,
    -- | Of the inputs, by name of their node in the lock file.
    inputNarHashes :: ![Text]
  }
  deriving (Eq, Show)

toLockedFlake :: Metadata -> LockedFlake
toLockedFlake m = LockedFlake m.url $ m.narHash : m.inputNarHashes

parseMetadata :: ByteString -> Either String Metadata
parseMetadata bs = eitherDecodeStrict bs >>= parseEither pMetadata

pMetadata :: Value -> Parser Metadata
pMetadata = withObject "flake metadata" $ \o -> do
  url <- o .: "url"
  narHash <- o .: "locked" >>= withObject "locked reference" (.: "narHash")
  nodes <- o .: "locks" >>= withObject "lock file" (.: "nodes")
  -- The root node, the flake itself, has no locked reference.
  inputs <- traverse pNode $ KeyMap.toAscList nodes
  pure $ Metadata url narHash $ catMaybes inputs
  where
    pNode (k, v) = withObject ("lock node " <> Key.toString k) pLocked v
    pLocked n =
      n .:? "locked" >>= \case
        Nothing -> pure Nothing
        Just l -> Just <$> withObject "locked reference" (.: "narHash") l
