-- |
-- Module      :  Magix.DirectivesSpec
-- Description :  Unit tests for Directive
-- Copyright   :  2024 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Fri Oct 18 09:34:01 2024.
module Magix.DirectivesSpec
  ( spec,
  )
where

import Data.Maybe (fromMaybe)
import Data.Text (Text, stripPrefix, unlines)
import Data.Text.IO (readFile)
import Magix.Directives (Directives (..), pDirectives, pLanguageDirectives, pMagixDirective, pNixpkgsDirective, pShebang)
import Magix.Language.Bash.Directives (BashDirectives (..))
import Magix.Language.Directives (LanguageDirectives (..))
import Magix.Language.Haskell.Directives (HaskellDirectives (..))
import Magix.Language.Language (Language (..), getLanguageNameLowercase)
import Magix.Language.Tools (getEmptyLanguageDirectives, getMinimalDirectiveInput, getMinimalTestcase)
import Magix.Tools (parseF, parseS)
import Test.Hspec (Spec, SpecWith, describe, it)
import Prelude hiding (readFile, unlines)

languages :: [Language]
languages = [minBound .. maxBound]

pEmptyLanguageDirectivesFor :: Language -> SpecWith ()
pEmptyLanguageDirectivesFor language =
  it description $
    mapM_
      testWith
      [ "#!magix " <> name,
        "#!magix " <> name <> "\t",
        "#!magix \t" <> name <> "\t",
        "#!magix \t" <> name <> "\n\n"
      ]
  where
    description = "parses basic " <> show language <> " language directives"
    name = getLanguageNameLowercase language
    emptyLanguageDirectives = getEmptyLanguageDirectives language
    testWith directives = parseS pLanguageDirectives directives emptyLanguageDirectives

pLanguageDirectivesFailFor :: Language -> SpecWith ()
pLanguageDirectivesFailFor language =
  it description $
    mapM_
      (parseF pLanguageDirectives)
      [ header <> "\n\n" <> directives,
        header <> " " <> directives,
        "#! magix " <> name <> "\n" <> directives,
        header <> "\n\n#!",
        header <> "\n\n#!foo"
      ]
  where
    description = "fails on wrong " <> show language <> " language directives"
    name = getLanguageNameLowercase language
    header = "#!magix " <> name
    directives = getMinimalDirectives language

-- | The directive lines of 'getMinimalDirectiveInput', without the Magix
-- directive.
getMinimalDirectives :: Language -> Text
getMinimalDirectives language =
  fromMaybe (error $ "getMinimalDirectives: no directives for " <> show language) $
    stripPrefix ("#!magix " <> getLanguageNameLowercase language <> "\n") input
  where
    (input, _) = getMinimalDirectiveInput language

pMinimalDirectivesFor :: Language -> SpecWith ()
pMinimalDirectivesFor language =
  it ("parses minimal " <> show language <> " language directives") $
    uncurry (parseS pLanguageDirectives) (getMinimalDirectiveInput language)

pMinimalFor :: Language -> SpecWith ()
pMinimalFor language =
  it "parses minimal sample scripts" $ do
    let (fn, res) = getMinimalTestcase language
    fo <- readFile fn
    parseS
      pDirectives
      fo
      ( Directives
          { nixpkgsRef = Nothing,
            language = res
          }
      )

spec :: Spec
spec = do
  describe "pShebang" $ do
    it "parses the shebang" $
      parseS pShebang "#!/usr/bin/env magix" "magix"

    it "fails on wrong shebangs" $ do
      parseF pShebang " #!/usr/bin/env magix"
      parseF pShebang "#! /usr/bin/env magix"
      parseF pShebang "#!/usr/bin/env magis"
      parseF pShebang "#!/usr/bin/env"
      parseF pShebang "#/usr/bin/env magix"
      parseF pShebang "#/usr/bin/env3magix"

  describe "pMagixDirective" $ do
    it "parses Magix directives" $ do
      let testLanguage lang =
            parseS pMagixDirective ("#!magix " <> getLanguageNameLowercase lang) lang
      sequence_ [testLanguage lang | lang <- languages]

    it "fails on wrong Magix directives" $ do
      parseF pMagixDirective "#!magic foo"
      parseF pMagixDirective "#!magic"

  describe "pLanguageDirectives" $ do
    it "fails on wrong language directives" $ do
      parseF pLanguageDirectives "#! bar"
      parseF pLanguageDirectives "#!magix foo\n\n#!packages bar"
      parseF pLanguageDirectives "#!magix unknown"

    mapM_ pLanguageDirectivesFailFor languages

    mapM_ pEmptyLanguageDirectivesFor languages

    mapM_ pMinimalDirectivesFor languages

    it "parses Haskell directives with GHC flags only" $
      parseS pLanguageDirectives "#!magix haskell\n#!ghcFlags -threaded" $
        HaskellD (HaskellDirectives [] ["-threaded"])

  describe "pDirectives" $ do
    mapM_ pMinimalFor languages

    it "parses some edge cases" $ do
      let spaceTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                "#!packages a ",
                ""
              ]
      parseS
        pDirectives
        spaceTest
        ( Directives
            { nixpkgsRef = Nothing,
              language = BashD (BashDirectives ["a"])
            }
        )
      let newlineTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                "#!packages a",
                ""
              ]
      parseS
        pDirectives
        newlineTest
        ( Directives
            { nixpkgsRef = Nothing,
              language = BashD (BashDirectives ["a"])
            }
        )
      let newlineEmptyTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                ""
              ]
      parseS
        pDirectives
        newlineEmptyTest
        ( Directives
            { nixpkgsRef = Nothing,
              language = BashD (BashDirectives [])
            }
        )
      let emptySpaceTest :: Text =
            unlines
              [ "#!/usr/bin/env \t magix\t ",
                "#!magix \t bash \t"
              ]
      parseS
        pDirectives
        emptySpaceTest
        ( Directives
            { nixpkgsRef = Nothing,
              language = BashD (BashDirectives [])
            }
        )

    it "fails on some edge cases" $
      let directiveNotNewlineTest :: Text =
            unlines
              ["#!/usr/bin/env \t magix\t #!magix \t bash \t"]
       in parseF pDirectives directiveNotNewlineTest

    it "parses a Nixpkgs directive before language directives" $ do
      let script :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!nixpkgs github:NixOS/nixpkgs/nixos-unstable",
                "#!magix bash",
                "#!packages jq"
              ]
      parseS
        pDirectives
        script
        ( Directives
            { nixpkgsRef = Just "github:NixOS/nixpkgs/nixos-unstable",
              language = BashD (BashDirectives ["jq"])
            }
        )

    it "returns Nothing for Nixpkgs reference when directive is absent" $ do
      let script :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                "#!packages jq"
              ]
      parseS
        pDirectives
        script
        ( Directives
            { nixpkgsRef = Nothing,
              language = BashD (BashDirectives ["jq"])
            }
        )

  describe "pNixpkgsDirective" $ do
    it "parses a Nixpkgs Flake ref" $ do
      parseS pNixpkgsDirective "#!nixpkgs github:NixOS/nixpkgs/nixos-unstable" "github:NixOS/nixpkgs/nixos-unstable"
      parseS pNixpkgsDirective "#!nixpkgs nixpkgs" "nixpkgs"
      parseS pNixpkgsDirective "#!nixpkgs github:NixOS/nixpkgs/nixos-24.05" "github:NixOS/nixpkgs/nixos-24.05"
    it "fails on missing value" $
      parseF pNixpkgsDirective "#!nixpkgs"
    it "fails on wrong directive name" $
      parseF pNixpkgsDirective "#!nixpkgspath /nix/store/foo"
