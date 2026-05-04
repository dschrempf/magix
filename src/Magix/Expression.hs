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
    getFlakeWrapper,
  )
where

import Data.Foldable qualified as Foldable
import Data.Text (Text, replace)
import Data.Text.IO (readFile)
import Magix.Config (Config (..))
import Magix.Language.Common.Expression (Replacement, getCommonReplacements)
import Magix.Language.Directives (LanguageDirectives, getLanguage)
import Magix.Language.Expression (getLanguageReplacements)
import Magix.Language.Language (Language (..))
import Paths_magix (getDataFileName)
import Prelude hiding (readFile)

getTemplatePath :: Language -> FilePath
getTemplatePath language = "src/Magix/Language/" <> show language <> "/Template.nix"

getTemplate :: Language -> IO Text
getTemplate language = getDataFileName (getTemplatePath language) >>= readFile

getReplacements :: Config -> LanguageDirectives -> [Replacement]
getReplacements c ds = getCommonReplacements c ++ getLanguageReplacements ds

getNixExpression :: Config -> LanguageDirectives -> IO Text
getNixExpression c ds = do
  t <- getTemplate $ getLanguage ds
  pure $ Foldable.foldl' replace' t (getReplacements c ds)
  where
    replace' t (x, y) = replace x y t

-- | Generate the universal Flake wrapper with the Nixpkgs reference
--   substituted.
getFlakeWrapper :: Text -> IO Text
getFlakeWrapper ref = do
  t <- getDataFileName "src/Magix/FlakeTemplate.nix" >>= readFile
  pure $ replace "__NIXPKGS_REF__" ref t
