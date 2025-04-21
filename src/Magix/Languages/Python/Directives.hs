-- |
-- Module      :  Magix.Languages.Python.Directives
-- Description :  Python directives for Magix
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:17:40 2024.
module Magix.Languages.Python.Directives
  ( PythonDirectives (..),
    pPythonDirectives,
  )
where

import Data.Text (Text)
import Magix.Languages.Directives
  ( Parser,
    pDirectiveWithValues,
    pManyDirectives,
  )
import Prelude hiding (readFile)

newtype PythonDirectives = PythonDirectives {_pythonPackages :: [Text]}
  deriving (Eq, Show, Semigroup, Monoid)

pPythonDirectives :: Parser PythonDirectives
pPythonDirectives =
  pManyDirectives $
    PythonDirectives <$> pDirectiveWithValues "pythonPackages"
