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
    Options (..),
    Rebuild (..),
    Verbosity (..),
    getOptions,
    -- Configuration.
    Config,
    getConfig,
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
    run,
    -- Languages.
    Directives,
  )
where

import Magix.Build (BuildStatus (..), build, getBuildStatus, withBuildLock)
import Magix.Config (Config, getConfig)
import Magix.Directives (getDirectives)
import Magix.Expression (getNixExpression)
import Magix.Languages.Directives (Directives)
import Magix.Options (Options (..), Rebuild (..), Verbosity (..), getOptions)
import Magix.Run (run)
