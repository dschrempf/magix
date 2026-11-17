-- |
-- Module      :  Magix.NixpkgsPath
-- Description :  Get path to Nixpkgs
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Oct 24 06:47:24 2024.
module Magix.NixpkgsPath
  ( NixPathEntry (..),
    NixPathTarget (..),
    renderNixPathEntry,
    pNixPathEntry,
    pNixPath,
    getDefaultNixpkgsPath,
    EntryError (..),
    renderEntryError,
    resolveNixpkgs,
  )
where

import Control.Applicative (optional)
import Control.Exception (Exception)
import Control.Monad (void)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import Data.List (intercalate, isPrefixOf, stripPrefix)
import Data.Void (Void)
import Magix.Env (NixEnv (..), nixEnvDesc)
import System.Directory (doesPathExist)
import System.FilePath ((</>))
import Text.Megaparsec
  ( MonadParsec (takeWhile1P, takeWhileP),
    Parsec,
    eof,
    errorBundlePretty,
    getInput,
    parse,
    sepEndBy,
  )
import Text.Megaparsec.Char (char)

type Parser = Parsec Void String

-- | A single entry of @NIX_PATH@.
--
-- Entries are either prefixed (@prefix=target@, e.g. @nixpkgs=/path@) or
-- unprefixed. For an unprefixed directory @dir@, a lookup @<x>@ resolves to
-- @dir/x@ (see @builtins.findFile@).
data NixPathEntry
  = Prefixed !String !NixPathTarget
  | Unprefixed !NixPathTarget
  deriving (Eq, Show)

-- | What a @NIX_PATH@ entry points to, classified like Nix does.
data NixPathTarget
  = TargetPath !FilePath
  | -- | The flake reference after @flake:@.
    TargetFlake !String
  | -- | A @channel:@ or URL target, which Nix downloads.
    TargetRemote !String
  deriving (Eq, Show)

renderNixPathTarget :: NixPathTarget -> String
renderNixPathTarget (TargetPath p) = p
renderNixPathTarget (TargetFlake r) = flakePrefix <> r
renderNixPathTarget (TargetRemote u) = u

renderNixPathEntry :: NixPathEntry -> String
renderNixPathEntry (Prefixed p t) = p <> "=" <> renderNixPathTarget t
renderNixPathEntry (Unprefixed t) = renderNixPathTarget t

flakePrefix :: String
flakePrefix = "flake:"

-- | URL schemes Nix downloads (@EvalSettings::isPseudoUrl@).
urlSchemes :: [String]
urlSchemes = ["http", "https", "file", "channel", "git", "s3", "ssh"]

isRemote :: String -> Bool
isRemote x = "channel:" `isPrefixOf` x || any (\s -> (s <> "://") `isPrefixOf` x) urlSchemes

classifyTarget :: String -> NixPathTarget
classifyTarget x
  | isRemote x = TargetRemote x
  | Just r <- stripPrefix flakePrefix x = TargetFlake r
  | otherwise = TargetPath x

-- | Characters allowed inside a @NIX_PATH@ entry (colon and whitespace separate
-- entries).
isEntryChar :: Char -> Bool
isEntryChar x = x /= ':' && not (isSpace x)

-- | Separator between @NIX_PATH@ entries.
pSep :: Parser ()
pSep = void $ takeWhile1P (Just "separator") (\x -> x == ':' || isSpace x)

-- | Whether the colon following the text of an entry so far belongs to the
-- entry, given the text after the colon.
--
-- Like Nix (@EvalSettings::parseNixPath@), the colon belongs to the entry if the
-- target (the text after the last @=@) is @flake@ or @channel@, or a URL scheme
-- followed by @//@. Nix then reads up to the next colon, so a target holding
-- more colons, such as @flake:github:NixOS/nixpkgs@ or
-- @https://host:8080/x@, is split anyway.
joinsColon :: String -> String -> Bool
joinsColon soFar after =
  target `elem` ["flake", "channel"]
    || (target `elem` urlSchemes && "//" `isPrefixOf` after)
  where
    target = reverse $ takeWhile (/= '=') $ reverse soFar

-- | Parse a single @NIX_PATH@ entry and classify it.
pNixPathEntry :: Parser NixPathEntry
pNixPathEntry = do
  tok <- takeWhile1P (Just "entry") isEntryChar
  input <- getInput
  rest <- case input of
    ':' : after
      | joinsColon tok after -> (':' :) <$> (char ':' *> takeWhileP (Just "entry") isEntryChar)
    _ -> pure ""
  pure $ classify $ tok <> rest
  where
    -- Nix splits an entry at its first '='.
    classify x = case break (== '=') x of
      (prefix, '=' : target) -> Prefixed prefix $ classifyTarget target
      _ -> Unprefixed $ classifyTarget x

-- | Parse the value of @NIX_PATH@ into its entries.
pNixPath :: Parser [NixPathEntry]
pNixPath = optional pSep *> sepEndBy pNixPathEntry pSep <* eof

data NixpkgsPathError = NixpkgsPathError
  { _nixPath :: !String,
    _err :: !String
  }
  deriving (Eq, Show)

instance Exception NixpkgsPathError

-- | Parse the entries of @NIX_PATH@ (see 'resolveNixpkgs' for resolving one to a
-- Nixpkgs path).
getDefaultNixpkgsPath :: String -> Either NixpkgsPathError [NixPathEntry]
getDefaultNixpkgsPath nixPath =
  first (NixpkgsPathError nixPath . errorBundlePretty) $ parse pNixPath "" nixPath

-- | A @NIX_PATH@ entry that may provide Nixpkgs, but magix cannot resolve.
data EntryError
  = -- | A @channel:@ or URL target.
    RemoteEntry !NixPathEntry
  | -- | A @flake:@ target the flake registries do not resolve to a path, and
    -- why.
    FlakeEntry !NixPathEntry !String
  deriving (Eq, Show)

renderEntryError :: EntryError -> String
renderEntryError (RemoteEntry e) =
  cannotUse e
    <> ": magix does not download Nixpkgs. "
    <> fixHint ["or use '--nixpkgs-path'"]
renderEntryError (FlakeEntry e why) =
  cannotUse e
    <> ": "
    <> why
    <> ". "
    <> fixHint
      [ "use '--nixpkgs-path'",
        "or point the flake id to a path in a flake registry (e.g. 'nix registry add <id> /nix/store/...-source')"
      ]

cannotUse :: NixPathEntry -> String
cannotUse e = "Cannot use the " <> nixPathVar <> " entry '" <> renderNixPathEntry e <> "'"

-- | Suggest a Nixpkgs path in @NIX_PATH@, and the given alternatives.
fixHint :: [String] -> String
fixHint alternatives =
  intercalate ", " (("Put a Nixpkgs path in " <> nixPathVar <> " (e.g. 'nixpkgs=/nix/store/...-source')") : alternatives)
    <> "."

nixPathVar :: String
nixPathVar = fst nixEnvDesc.nixPath

-- | Resolve a single @NIX_PATH@ entry to a Nixpkgs path, if it provides one.
--
-- A @nixpkgs=<path>@ entry resolves to @<path>@. An unprefixed directory @dir@
-- resolves to @dir/nixpkgs@ if that path exists. A @flake:<ref>@ target
-- resolves to a path through the given function, called for such a target only.
resolveNixpkgs ::
  (String -> IO (Either String FilePath)) ->
  NixPathEntry ->
  IO (Either EntryError (Maybe FilePath))
resolveNixpkgs resolveFlake e = case e of
  Prefixed "nixpkgs" t -> fmap Just <$> resolveTarget t
  Prefixed _ _ -> pure $ Right Nothing
  Unprefixed t -> resolveTarget t >>= traverse findIn
  where
    resolveTarget (TargetPath p) = pure $ Right p
    resolveTarget (TargetFlake r) = first (FlakeEntry e) <$> resolveFlake r
    resolveTarget (TargetRemote _) = pure $ Left $ RemoteEntry e
    findIn dir = do
      let candidate = dir </> "nixpkgs"
      exists <- doesPathExist candidate
      pure $ if exists then Just candidate else Nothing
