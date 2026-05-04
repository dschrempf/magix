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
  ( getEmptyLanguageDirectives,
    getMinimalTestcase,
  )
where

import Magix.Languages.Bash.Directives (BashDirectives (..))
import Magix.Languages.Directives (LanguageDirectives (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Magix.Languages.Language (Language (..))
import Magix.Languages.Python.Directives (PythonDirectives (..))
import Prelude hiding (readFile)

getEmptyLanguageDirectives :: Language -> LanguageDirectives
getEmptyLanguageDirectives Bash = BashD mempty
getEmptyLanguageDirectives Haskell = HaskellD mempty
getEmptyLanguageDirectives Python = PythonD mempty

getMinimalTestcase :: Language -> (FilePath, LanguageDirectives)
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
