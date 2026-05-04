-- |
-- Module      :  Magix
-- Description :  Barrel file with re-exports
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 15:29:10 2024.
module Magix
  ( -- Options.
    Command (..),
    Options (..),
    CleanOptions (..),
    Rebuild (..),
    Verbosity (..),
    getCommand,
    -- Environment.
    MagixEnv (..),
    NixEnv (..),
    -- Configuration.
    BuildMode (..),
    Config,
    getConfig,
    getCacheDir,
    -- Directives.
    getDirectives,
    -- Expression.
    getNixExpression,
    -- Build.
    BuildStatus (..),
    getBuildStatus,
    build,
    withBuildLock,
    -- Run.
    runScript,
    -- Languages.
    Directives (..),
    LanguageDirectives,
  )
where

import Magix.Build (BuildStatus (..), build, getBuildStatus, withBuildLock)
import Magix.BuildMode (BuildMode (..))
import Magix.Config (Config, getCacheDir, getConfig)
import Magix.Directives (Directives (..), getDirectives)
import Magix.Env (MagixEnv (..), NixEnv (..))
import Magix.Expression (getNixExpression)
import Magix.Language.Directives (LanguageDirectives)
import Magix.Options
  ( CleanOptions (..),
    Command (..),
    Options (..),
    Rebuild (..),
    Verbosity (..),
    getCommand,
  )
import Magix.Run (runScript)
