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
    pNixPathEntry,
    pNixPath,
    getDefaultNixpkgsPath,
    resolveNixpkgs,
  )
where

import Control.Applicative (optional)
import Control.Exception (Exception)
import Control.Monad (void)
import Data.Bifunctor (Bifunctor (first))
import Data.Char (isSpace)
import Data.Void (Void)
import System.Directory (doesPathExist)
import System.FilePath ((</>))
import Text.Megaparsec
  ( MonadParsec (takeWhile1P),
    Parsec,
    eof,
    errorBundlePretty,
    parse,
    sepEndBy,
  )

type Parser = Parsec Void String

-- | A single entry of @NIX_PATH@.
--
-- Entries are either prefixed (@prefix=path@, e.g. @nixpkgs=/path@) or
-- unprefixed directories. For an unprefixed directory @dir@, a lookup @<x>@
-- resolves to @dir/x@ (see @builtins.findFile@).
data NixPathEntry
  = Prefixed !String !FilePath
  | Unprefixed !FilePath
  deriving (Eq, Show)

-- | Characters allowed inside a @NIX_PATH@ entry (colon and whitespace separate
-- entries).
isEntryChar :: Char -> Bool
isEntryChar x = x /= ':' && not (isSpace x)

-- | Separator between @NIX_PATH@ entries.
pSep :: Parser ()
pSep = void $ takeWhile1P (Just "separator") (\x -> x == ':' || isSpace x)

-- | Parse a single @NIX_PATH@ entry and classify it as prefixed or unprefixed.
pNixPathEntry :: Parser NixPathEntry
pNixPathEntry = classify <$> takeWhile1P (Just "entry") isEntryChar
  where
    classify tok = case break (== '=') tok of
      (prefix, '=' : path) -> Prefixed prefix path
      _ -> Unprefixed tok

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

-- | Resolve a single @NIX_PATH@ entry to a Nixpkgs path, if it provides one.
--
-- A @nixpkgs=<path>@ entry resolves to @<path>@. An unprefixed directory @dir@
-- resolves to @dir/nixpkgs@ if that path exists.
resolveNixpkgs :: NixPathEntry -> IO (Maybe FilePath)
resolveNixpkgs (Prefixed "nixpkgs" p) = pure $ Just p
resolveNixpkgs (Prefixed _ _) = pure Nothing
resolveNixpkgs (Unprefixed dir) = do
  let candidate = dir </> "nixpkgs"
  exists <- doesPathExist candidate
  pure $ if exists then Just candidate else Nothing
