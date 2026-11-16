-- |
-- Module      :  Magix.Run
-- Description :  Run the built executable
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Oct 20 10:53:12 2024.
module Magix.Run
  ( runScript,
  )
where

import Magix.Config (Config (..))
import Magix.Options (Options (scriptArgs))
import System.FilePath ((</>))
import System.Posix.Process (executeFile)

-- | Run the script, given the store path of its build result.
runScript :: Options -> Config -> FilePath -> IO a
runScript opts conf result = executeFile executable False (scriptArgs opts) Nothing
  where
    executable = result </> "bin" </> scriptName conf
