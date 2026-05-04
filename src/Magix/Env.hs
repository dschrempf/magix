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
  { nixPath :: !a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

nixEnvDesc :: NixEnv (String, [String])
nixEnvDesc =
  NixEnv
    { nixPath =
        ( "NIX_PATH",
          [ "Used to extract the default Nixpkgs channel path when neither '--nixpkgs-path' nor 'MAGIX_NIXPKGS_REF' is set."
          ]
        )
    }

getNixEnv :: IO (NixEnv (Maybe String))
getNixEnv = traverse (lookupEnv . fst) nixEnvDesc
