-- |
-- Module      :  Magix.Language.Tools
-- Description :  Language-specific test tools
-- Copyright   :  2025 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Apr 22 15:19:11 2025.
module Magix.Language.Tools
  ( getEmptyLanguageDirectives,
    getMinimalTestcase,
    getMinimalDirectiveInput,
    pManyDirectives,
  )
where

import Control.Applicative (many)
import Data.Text (Text)
import Magix.Language.Bash.Directives (BashDirectives (..))
import Magix.Language.Common.Directives (Parser, pLineEnd)
import Magix.Language.Directives (LanguageDirectives (..))
import Magix.Language.Haskell.Directives (HaskellDirectives (..))
import Magix.Language.Language (Language (..))
import Magix.Language.Python.Directives (PythonDirectives (..))
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

getMinimalDirectiveInput :: Language -> (Text, LanguageDirectives)
getMinimalDirectiveInput Bash =
  ( "#!magix bash\n#!packages bar",
    BashD (BashDirectives ["bar"])
  )
getMinimalDirectiveInput Haskell =
  ( "#!magix haskell\n#!haskellPackages bytestring\n#!ghcFlags -threaded",
    HaskellD (HaskellDirectives ["bytestring"] ["-threaded"])
  )
getMinimalDirectiveInput Python =
  ( "#!magix python\n#!pythonPackages requests",
    PythonD (PythonDirectives ["requests"])
  )

-- | Directive lines, each ending with 'pLineEnd'.
pManyDirectives :: (Monoid b) => Parser b -> Parser b
pManyDirectives p = mconcat <$> many (p <* pLineEnd)
