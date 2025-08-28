-- |
-- Module      :  Magix.Languages.Tools
-- Description :  Language-specific test tools
-- Copyright   :  2025 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Apr 22 15:19:11 2025.
module Magix.Languages.Tools
  ( getEmptyDirectives,
    getMinimalTestcase,
  )
where

import Magix.Languages.Bash.Directives (BashDirectives (BashDirectives))
import Magix.Languages.Directives (Directives (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (HaskellDirectives))
import Magix.Languages.Language (Language (..))
import Magix.Languages.Python.Directives (PythonDirectives (PythonDirectives))
import Prelude hiding (readFile)

getEmptyDirectives :: Language -> Directives
getEmptyDirectives Bash = BashD mempty
getEmptyDirectives Haskell = HaskellD mempty
getEmptyDirectives Python = PythonD mempty

getMinimalTestcase :: Language -> (FilePath, Directives)
getMinimalTestcase Bash =
  ( "test-scripts/bash/minimal",
    BashD (BashDirectives ["jq"])
  )
getMinimalTestcase Haskell =
  ( "test-scripts/haskell/minimal",
    HaskellD (HaskellDirectives ["bytestring"] ["-threaded"])
  )
getMinimalTestcase Python =
  ( "test-scripts/python/minimal",
    PythonD (PythonDirectives ["numpy"])
  )
