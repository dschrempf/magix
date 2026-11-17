-- |
-- Module      :  Magix.Env
-- Description :  Environment variables used by Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun May  3 00:00:00 2026.
module Magix.Env
  ( -- * Magix-specific environment
    MagixEnv (..),
    magixEnvDesc,
    getMagixEnv,

    -- * Nix environment
    NixEnv (..),
    nixEnvDesc,
    getNixEnv,
  )
where

import System.Environment (lookupEnv)

{-------------------------------------------------------------------------------
  Magix-specific environment
-------------------------------------------------------------------------------}

-- | Magix-specific environment variables.
data MagixEnv a = MagixEnv
  { nixpkgsRef :: !a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

magixEnvDesc :: MagixEnv (String, [String])
magixEnvDesc =
  MagixEnv
    { nixpkgsRef =
        ( "MAGIX_NIXPKGS_REF",
          [ "Nixpkgs Flake reference; overridden by '--nixpkgs-ref'.",
            "When set, uses 'nix build' instead of 'nix-build'."
          ]
        )
    }

getMagixEnv :: IO (MagixEnv (Maybe String))
getMagixEnv = traverse (lookupEnv . fst) magixEnvDesc

{-------------------------------------------------------------------------------
  Nix environment
-------------------------------------------------------------------------------}

-- | Nix-related environment variables used by Magix.
data NixEnv a = NixEnv
  { nixPath :: !a,
    nixConfigHome :: !a,
    xdgConfigHome :: !a,
    nixConfDir :: !a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

nixEnvDesc :: NixEnv (String, [String])
nixEnvDesc =
  NixEnv
    { nixPath =
        ( "NIX_PATH",
          [ "Used to find Nixpkgs when neither '--nixpkgs-path' nor 'MAGIX_NIXPKGS_REF' is set: the first entry providing it, 'nixpkgs=<path>' or an unprefixed directory '<dir>' holding '<dir>/nixpkgs'.",
            "A 'flake:<id>' target resolves through the user and the system flake registry to the path of a 'path' entry. magix does not read the global registry, and fails on other targets, such as 'channel:' and URL targets."
          ]
        ),
      nixConfigHome =
        ( "NIX_CONFIG_HOME",
          ["Directory of the user flake registry (default: '$XDG_CONFIG_HOME/nix', else '$HOME/.config/nix')."]
        ),
      xdgConfigHome =
        ( "XDG_CONFIG_HOME",
          ["See 'NIX_CONFIG_HOME'."]
        ),
      nixConfDir =
        ( "NIX_CONF_DIR",
          ["Directory of the system flake registry (default: '/etc/nix')."]
        )
    }

getNixEnv :: IO (NixEnv (Maybe String))
getNixEnv = traverse (lookupEnv . fst) nixEnvDesc
