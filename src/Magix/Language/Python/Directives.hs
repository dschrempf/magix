-- |
-- Module      :  Magix.Language.Python.Directives
-- Description :  Python directives for Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Language.Python.Directives
  ( PythonDirectives (..),
    pPythonDirective,
  )
where

import Data.Text (Text)
import Magix.Language.Common.Directives
  ( Parser,
    pDirectiveWithValues,
  )
import Prelude hiding (readFile)

newtype PythonDirectives = PythonDirectives {_pythonPackages :: [Text]}
  deriving (Eq, Show, Semigroup, Monoid)

-- | One Python directive line, without its end.
pPythonDirective :: Parser PythonDirectives
pPythonDirective = PythonDirectives <$> pDirectiveWithValues "pythonPackages"
