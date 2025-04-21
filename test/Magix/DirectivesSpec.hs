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

import Data.Text (Text, unlines)
import Data.Text.IO (readFile)
import Magix.Directives
  ( Directives (..),
    pDirectives,
    pLanguageDirectives,
    pMagixDirective,
    pShebang,
  )
import Magix.Language (Language (..), getLanguageNameLowercase)
import Magix.Languages.Bash.Directives (BashDirectives (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Magix.Languages.Python.Directives (PythonDirectives (..))
import Magix.Tools (parseF, parseS)
import Test.Hspec (Spec, SpecWith, describe, it, shouldBe)
import Text.Megaparsec (parse)
import Prelude hiding (readFile, unlines)

fnMinimalBash :: FilePath
fnMinimalBash = "test-scripts/bash/minimal"

readMinimalBash :: IO Text
readMinimalBash = readFile fnMinimalBash

fnMinimalHaskell :: FilePath
fnMinimalHaskell = "test-scripts/haskell/minimal"

readMinimalHaskell :: IO Text
readMinimalHaskell = readFile fnMinimalHaskell

fnMinimalPython :: FilePath
fnMinimalPython = "test-scripts/python/minimal"

readMinimalPython :: IO Text
readMinimalPython = readFile fnMinimalPython

languages :: [Language]
languages = [minBound .. maxBound]

getEmptyDirectives :: Language -> Directives
getEmptyDirectives Bash = BashD mempty
getEmptyDirectives Haskell = HaskellD mempty
getEmptyDirectives Python = PythonD mempty

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
    emptyDirectives = getEmptyDirectives language
    testWith directives = parseS pLanguageDirectives directives emptyDirectives

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
      parseF pMagixDirective "#!magic haskell"
      parseF pMagixDirective "#!magic"

  describe "pLanguageDirectives" $ do
    it "fails on wrong language directives" $ do
      parseF pLanguageDirectives "#! bar"
      parseF pLanguageDirectives "#!magix foo\n\n#!packages bar"
      parseF pLanguageDirectives "#!magix bash\n\n#!packages bar"
      parseF pLanguageDirectives "#!magix bash #!packages bar"
      parseF pLanguageDirectives "#! magix bash\n#!packages bar"
      parseF pLanguageDirectives "#!magix bash\n\n#!"
      parseF pLanguageDirectives "#!magix bash\n\n#!foo"
      parseF pLanguageDirectives "#!magix unknown"

    mapM_ pEmptyLanguageDirectivesFor languages

    -- Force testing of all languages.
    it "parses Bash language directives" $ do
      parseS pLanguageDirectives "#!magix bash\n#!packages bar" $
        BashD (BashDirectives ["bar"])

    it "parses Haskell directives" $ do
      parseS pLanguageDirectives "#!magix haskell\n#!ghcFlags -threaded" $
        HaskellD (HaskellDirectives [] ["-threaded"])
      parseS pLanguageDirectives "#!magix haskell\n#!haskellPackages bytestring\n#!ghcFlags -threaded" $
        HaskellD (HaskellDirectives ["bytestring"] ["-threaded"])

    it "parses Python directives" $ do
      parseS pLanguageDirectives "#!magix python\n#!pythonPackages requests" $
        PythonD (PythonDirectives ["requests"])

  describe "pDirectives" $ do
    -- Force testing of all languages.
    it "parses minimal sample scripts" $ do
      minimalBash <- readMinimalBash
      parse pDirectives fnMinimalBash minimalBash
        `shouldBe` Right (BashD (BashDirectives ["jq"]))
      minimalHaskell <- readMinimalHaskell
      parse pDirectives fnMinimalHaskell minimalHaskell
        `shouldBe` Right (HaskellD (HaskellDirectives ["bytestring"] ["-threaded"]))
      minimalPython <- readMinimalPython
      parse pDirectives fnMinimalPython minimalPython
        `shouldBe` Right (PythonD (PythonDirectives ["numpy"]))

    it "parses some edge cases" $ do
      let spaceTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                "#!packages a ",
                ""
              ]
      parseS pDirectives spaceTest $ BashD (BashDirectives ["a"])
      let newlineTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                "#!packages a",
                ""
              ]
      parseS pDirectives newlineTest $ BashD (BashDirectives ["a"])
      let newlineEmptyTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                ""
              ]
      parseS pDirectives newlineEmptyTest $ BashD (BashDirectives [])
      let emptySpaceTest :: Text =
            unlines
              [ "#!/usr/bin/env \t magix\t ",
                "#!magix \t bash \t"
              ]
      parseS pDirectives emptySpaceTest $ BashD (BashDirectives [])

    it "fails on some edge cases" $
      let directiveNotNewlineTest :: Text =
            unlines
              ["#!/usr/bin/env \t magix\t #!magix \t bash \t"]
       in parseF pDirectives directiveNotNewlineTest
