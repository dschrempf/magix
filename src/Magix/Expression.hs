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
import Magix.Languages.Common.Expression (Replacement, getCommonReplacements)
import Magix.Languages.Directives (Directives (..), getLanguage)
import Magix.Languages.Expression (getLanguageReplacements)
import Magix.Languages.Language (Language (..))
import Paths_magix (getDataFileName)
import Prelude hiding (readFile)

getTemplatePath :: Language -> FilePath
getTemplatePath language = "src/Magix/Languages/" <> show language <> "/Template.nix"

getTemplate :: Language -> IO Text
getTemplate language = getDataFileName (getTemplatePath language) >>= readFile

getReplacements :: Config -> Directives -> [Replacement]
getReplacements c ds = getCommonReplacements c ++ getLanguageReplacements ds

getNixExpression :: Config -> Directives -> IO Text
getNixExpression c ds = do
  t <- getTemplate $ getLanguage ds
  pure $ foldl' replace' t (getReplacements c ds)
  where
    replace' t (x, y) = replace x y t
