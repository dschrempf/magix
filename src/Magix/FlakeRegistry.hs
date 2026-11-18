-- |
-- Module      :  Magix.FlakeRegistry
-- Description :  Resolve flake ids through the local flake registries
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 17 10:00:00 2026.
--
-- Nix resolves a @flake:<id>@ entry of @NIX_PATH@ through its flake registries.
-- magix reads the user and the system registry, like Nix, but not the global
-- one, which Nix downloads; reading the files costs less than calling Nix.
module Magix.FlakeRegistry
  ( -- * Registries
    FlakeRef (..),
    PathFlake (..),
    renderPathFlake,
    RegistryEntry (..),
    Registry (..),
    parseRegistry,

    -- * Resolution
    FlakeError (..),
    renderFlakeError,
    parseFlakeId,
    resolveFlakeId,

    -- * Files
    RegistryFiles (..),
    getRegistryFiles,
    readRegistry,
    resolveRegistryId,
    resolveFlakeRef,
  )
where

import Control.Exception (IOException, try)
import Control.Monad (unless)
import Data.Aeson (Value (..), eitherDecodeStrict, encode, parseJSON, withObject, (.:))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Aeson.Types (Object, Parser, parseEither, parseMaybe)
import Data.Bifunctor (bimap, first)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Char (isAlpha, isAlphaNum, isAscii)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TL
import Magix.Env (NixEnv (..))
import System.Directory (getHomeDirectory)
import System.FilePath (isAbsolute)
import System.IO.Error (isDoesNotExistError)
import Text.Printf (printf)

-- | A flake reference, as far as magix resolves it.
data FlakeRef
  = -- | An indirect reference by a bare id, without ref or rev.
    IndirectRef !Text
  | PathRef !PathFlake
  | -- | Any other reference, as JSON.
    OtherRef !Text
  deriving (Eq, Show)

-- | A flake reference of type @path@.
data PathFlake = PathFlake
  { -- | Absolute.
    path :: !FilePath,
    -- | Such as @narHash@, @rev@ and @lastModified@; a number in decimal.
    attributes :: !(Map Text Text)
  }
  deriving (Eq, Show)

-- | The URL of the reference, like Nix renders it, e.g.
-- @path:/nix/store/…-source?lastModified=1794537146&narHash=sha256-Wpz/…%2B…%3D@.
renderPathFlake :: PathFlake -> Text
renderPathFlake f =
  "path:" <> percentEncode f.path <> query (Map.toAscList f.attributes)
  where
    query [] = ""
    query xs = "?" <> T.intercalate "&" [percentEncode (T.unpack k) <> "=" <> percentEncode (T.unpack v) | (k, v) <- xs]

-- | Percent-encode all but the unreserved characters and @/:\@@, like Nix
-- (@percentEncode@ with @allowedInQuery@).
percentEncode :: String -> Text
percentEncode = T.pack . concatMap enc . BS.unpack . TE.encodeUtf8 . T.pack
  where
    enc w
      | isAscii c && (isAlphaNum c || c `elem` ("-._~/:@" :: String)) = [c]
      | otherwise = printf "%%%02X" w
      where
        c = toEnum $ fromIntegral w

data RegistryEntry = RegistryEntry
  { from :: !FlakeRef,
    to :: !FlakeRef
  }
  deriving (Eq, Show)

data Registry = Registry
  { file :: !FilePath,
    entries :: ![RegistryEntry]
  }
  deriving (Eq, Show)

pFlakeRef :: Value -> Parser FlakeRef
pFlakeRef = withObject "flake reference" $ \o -> do
  t <- o .: "type"
  case t :: Text of
    -- A ref or rev makes it another reference.
    "indirect" | KeyMap.size o == 2 -> IndirectRef <$> o .: "id"
    "path" -> do
      p <- o .: "path"
      let attrs = traverse (parseMaybe pAttribute) $ KeyMap.toMapText $ foldr KeyMap.delete o ["type", "path"]
      pure $ case attrs of
        Just as | isAbsolute p -> PathRef $ PathFlake p as
        _ -> other o
    _ -> pure $ other o
  where
    other :: Object -> FlakeRef
    other = OtherRef . TL.toStrict . TL.decodeUtf8 . encode
    pAttribute :: Value -> Parser Text
    pAttribute (String x) = pure x
    pAttribute v = T.pack . show <$> (parseJSON v :: Parser Integer)

pRegistryEntry :: Value -> Parser RegistryEntry
pRegistryEntry = withObject "registry entry" $ \o ->
  RegistryEntry <$> (o .: "from" >>= pFlakeRef) <*> (o .: "to" >>= pFlakeRef)

pRegistryEntries :: Value -> Parser [RegistryEntry]
pRegistryEntries = withObject "flake registry" $ \o -> do
  v <- o .: "version"
  unless (v == (2 :: Int)) $ fail $ "unsupported version " <> show v
  o .: "flakes" >>= traverse pRegistryEntry

-- | Parse a flake registry (version 2) read from the given file.
parseRegistry :: FilePath -> ByteString -> Either FlakeError Registry
parseRegistry f bs =
  first (InvalidRegistry f) $
    Registry f <$> (eitherDecodeStrict bs >>= parseEither pRegistryEntries)

-- | Why a flake reference does not resolve to a path.
data FlakeError
  = -- | The reference after @flake:@ is no bare id.
    NotAnId !String
  | -- | A registry file is unreadable or malformed.
    InvalidRegistry !FilePath !String
  | -- | No registry has the id; the global one, which magix does not read, may.
    NotInRegistries !Text
  | -- | The id resolves to a reference other than a path, in the given file.
    NotAPath !Text !FilePath !Text
  | -- | The ids resolve to each other.
    Cycle ![Text]
  deriving (Eq, Show)

-- | Describe the error, given the registry files searched.
renderFlakeError :: [FilePath] -> FlakeError -> String
renderFlakeError fs err = case err of
  NotAnId r -> "'" <> r <> "' is no bare flake id such as 'nixpkgs'"
  InvalidRegistry f e -> "cannot read the flake registry " <> f <> ": " <> e
  NotInRegistries i ->
    "'" <> T.unpack i <> "' is in none of the flake registries " <> searched <> " (magix does not read the global one)"
  NotAPath i f r -> "'" <> T.unpack i <> "' resolves to " <> T.unpack r <> " in " <> f <> ", which is no path"
  Cycle is -> intercalate " -> " (map T.unpack is) <> " is a cycle in the flake registries " <> searched
  where
    searched = intercalate ", " fs

-- | Parse a bare flake id (@[a-zA-Z][a-zA-Z0-9_-]*@, Nix's @flakeIdRegex@).
parseFlakeId :: String -> Either FlakeError Text
parseFlakeId r = case r of
  c : cs | isIdStart c && all isIdChar cs -> Right $ T.pack r
  _ -> Left $ NotAnId r
  where
    isIdStart c = isAscii c && isAlpha c
    isIdChar c = isAscii c && (isAlphaNum c || c `elem` ['_', '-'])

-- | Resolve a flake id to a path through the registries, in order.
--
-- Like Nix, every step searches the registries from the first: an entry
-- resolving to another id restarts the search. Confirmed with
-- @nix-instantiate --find-file@: the user registry wins, a chain resolves
-- across registries, and a cycle fails.
resolveFlakeId :: [Registry] -> Text -> Either FlakeError PathFlake
resolveFlakeId registries = go []
  where
    go seen i
      | i `elem` seen = Left $ Cycle $ reverse $ i : seen
      | otherwise = case lookupId i of
          Nothing -> Left $ NotInRegistries i
          Just (_, PathRef p) -> Right p
          Just (_, IndirectRef j) -> go (i : seen) j
          Just (f, OtherRef r) -> Left $ NotAPath i f r
    lookupId i =
      listToMaybe
        [(r.file, e.to) | r <- registries, e <- r.entries, e.from == IndirectRef i]

-- | The registry files Nix reads, in its order.
data RegistryFiles = RegistryFiles
  { user :: !FilePath,
    system :: !FilePath
  }
  deriving (Eq, Show)

registryFileList :: RegistryFiles -> [FilePath]
registryFileList fs = [fs.user, fs.system]

-- | Locate the registry files like Nix 2.35, confirmed with @nix registry list@:
--
-- - User: @$NIX_CONFIG_HOME/registry.json@, else
--   @$XDG_CONFIG_HOME/nix/registry.json@, else
--   @$HOME/.config/nix/registry.json@. An empty variable counts as set.
--
-- - System: @$NIX_CONF_DIR/registry.json@, else @/etc/nix/registry.json@. An
--   empty variable counts as unset.
getRegistryFiles :: NixEnv (Maybe String) -> IO RegistryFiles
getRegistryFiles env = do
  userDir <- case (env.nixConfigHome, env.xdgConfigHome) of
    (Just d, _) -> pure d
    (Nothing, Just d) -> pure $ d <> "/nix"
    (Nothing, Nothing) -> (<> "/.config/nix") <$> getHomeDirectory
  let systemDir = case env.nixConfDir of
        Just d | not (null d) -> d
        _ -> "/etc/nix"
  -- Nix concatenates; '</>' would turn an empty directory into a relative path.
  pure $ RegistryFiles (userDir <> "/registry.json") (systemDir <> "/registry.json")

-- | Read a registry file; a missing one is empty, like in Nix.
readRegistry :: FilePath -> IO (Either FlakeError Registry)
readRegistry f = do
  r <- try $ BS.readFile f
  pure $ case r of
    Left e
      | isDoesNotExistError e -> Right $ Registry f []
      | otherwise -> Left $ InvalidRegistry f $ show (e :: IOException)
    Right bs -> parseRegistry f bs

-- | Resolve a flake id through the user and the system registry, and return
-- the registry files searched.
resolveRegistryId :: NixEnv (Maybe String) -> Text -> IO ([FilePath], Either FlakeError PathFlake)
resolveRegistryId env i = do
  files <- registryFileList <$> getRegistryFiles env
  registries <- traverse readRegistry files
  pure (files, sequence registries >>= (`resolveFlakeId` i))

-- | Resolve the reference of a @flake:@ entry to a path through the user and
-- the system registry. Fail with a message naming the registry files, also on
-- a malformed registry, which Nix skips with a warning.
resolveFlakeRef :: NixEnv (Maybe String) -> String -> IO (Either String FilePath)
resolveFlakeRef env r = do
  (files, result) <- case parseFlakeId r of
    Left e -> (,Left e) . registryFileList <$> getRegistryFiles env
    Right i -> resolveRegistryId env i
  pure $ bimap (renderFlakeError files) (.path) result
