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
    Language,
    getLanguageLowercase,
    pDirectives,
    pLanguageDirectives,
    pMagixDirective,
    pShebang,
  )
import Magix.Languages.Bash.Directives (BashDirectives (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
import Magix.Languages.Python.Directives (PythonDirectives (..))
import Magix.Tools (parseF, parseS)
import Test.Hspec (Spec, describe, it, shouldBe)
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
            parseS pMagixDirective ("#!magix " <> getLanguageLowercase lang) lang
      sequence_ [testLanguage lang | lang <- [minBound .. maxBound :: Language]]

    it "fails on wrong Magix directives" $ do
      parseF pMagixDirective "#!magic haskell"
      parseF pMagixDirective "#!magic"

  -- TODO: Here, we should only perform language-agnostic tests.
  -- Language-specific tests should all go into the `./Languages/` directory. I
  -- think the best would be to get a list of languages and corresponding empty
  -- directives, and test on this list.
  describe "pLanguageDirectives" $ do
    it "parses sample language directives" $ do
      let emptyBashDirectives = BashD $ BashDirectives []
      parseS pLanguageDirectives "#!magix bash\n#!packages bar" $ BashD (BashDirectives ["bar"])
      parseS pLanguageDirectives "#!magix bash" emptyBashDirectives
      parseS pLanguageDirectives "#!magix bash\t" emptyBashDirectives
      parseS pLanguageDirectives "#!magix \tbash\t" emptyBashDirectives
      parseS pLanguageDirectives "#!magix \tbash\n\n" emptyBashDirectives

    it "fails on wrong language directives" $ do
      parseF pLanguageDirectives "#! bar"
      parseF pLanguageDirectives "#!magix foo\n\n#!packages bar"
      parseF pLanguageDirectives "#!magix bash\n\n#!packages bar"
      parseF pLanguageDirectives "#!magix bash #!packages bar"
      parseF pLanguageDirectives "#! magix bash\n#!packages bar"
      parseF pLanguageDirectives "#!magix bash\n\n#!"
      parseF pLanguageDirectives "#!magix bash\n\n#!foo"
      parseF pLanguageDirectives "#!magix unknown"

    it "parses Haskell directives" $ do
      let emptyHaskellDirectives = HaskellD $ HaskellDirectives [] []
      parseS pLanguageDirectives "#!magix haskell" emptyHaskellDirectives
      parseS pLanguageDirectives "#!magix haskell\n#!ghcFlags -threaded" $
        HaskellD (HaskellDirectives [] ["-threaded"])
      parseS pLanguageDirectives "#!magix haskell\n#!haskellPackages bytestring\n#!ghcFlags -threaded" $
        HaskellD (HaskellDirectives ["bytestring"] ["-threaded"])

    it "parses Python directives" $ do
      let emptyPythonDirectives = PythonD $ PythonDirectives []
      parseS pLanguageDirectives "#!magix python" emptyPythonDirectives
      parseS pLanguageDirectives "#!magix python\n#!pythonPackages requests" $
        PythonD (PythonDirectives ["requests"])

  describe "pDirectives" $ do
    it "parses minimal sample scripts" $ do
      minimalBash <- readMinimalBash
      parse pDirectives fnMinimalBash minimalBash
        `shouldBe` Right (BashD (BashDirectives ["jq"]))
      minimalHaskell <- readMinimalHaskell
      parse pDirectives fnMinimalHaskell minimalHaskell
        `shouldBe` Right (HaskellD (HaskellDirectives ["bytestring"] ["-threaded"]))

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
