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
    getOptions,
    -- Configuration.
    getConfig,
    -- Directives.
    getDirectives,
    -- Expression.
    getNixExpression,
    -- Build.
    BuildStatus (..),
    getBuildStatus,
    build,
  )
where

import Magix.Build (BuildStatus (..), build, getBuildStatus)
import Magix.Config (getConfig)
import Magix.Directives (getDirectives)
import Magix.Expression (getNixExpression)
import Magix.Options (Options (..), getOptions)
