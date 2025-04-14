-- |
-- Module      :  Magix.Expression
-- Description :  Get Nix expressions
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 13:36:32 2024.
module Magix.Expression
  ( getTemplate,
    getReplacements,
    getNixExpression,
  )
where

import Data.Text (Text, replace)
import Data.Text.IO (readFile)
import Magix.Config (Config (..))
import Magix.Directives (Directives (..), getLanguageName)
import Magix.Languages.Bash.Expression (getBashReplacements)
import Magix.Languages.Common.Expression (Replacement, getCommonReplacements)
import Magix.Languages.Haskell.Expression (getHaskellReplacements)
import Magix.Languages.Python.Expression (getPythonReplacements)
import Paths_magix (getDataFileName)
import Prelude hiding (readFile)

getTemplatePath :: String -> FilePath
getTemplatePath languageName = "src/Magix/Languages/" <> languageName <> "/Template.nix"

getTemplate :: String -> IO Text
getTemplate languageName = getDataFileName (getTemplatePath languageName) >>= readFile

getLanguageReplacements :: Directives -> [Replacement]
getLanguageReplacements (Bash ds) = getBashReplacements ds
getLanguageReplacements (Haskell ds) = getHaskellReplacements ds
getLanguageReplacements (Python ds) = getPythonReplacements ds

getReplacements :: Config -> Directives -> [Replacement]
getReplacements c ds = getCommonReplacements c ++ getLanguageReplacements ds

getNixExpression :: Config -> Directives -> IO Text
getNixExpression c ds = do
  t <- getTemplate $ getLanguageName ds
  pure $ foldl' replace' t (getReplacements c ds)
  where
    replace' t (x, y) = replace x y t
