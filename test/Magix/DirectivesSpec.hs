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
import Magix.Languages.Bash.Directives (BashDirectives (..))
import Magix.Languages.Haskell.Directives (HaskellDirectives (..))
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
    it "parses sample Magix directives" $ do
      parseS pMagixDirective "#!magix haskell" "haskell"
      parseS pMagixDirective "#!magix foo" "foo"

    it "fails on wrong Magix directives" $ do
      parseF pMagixDirective "#!magic haskell"
      parseF pMagixDirective "#!magic"

  describe "pLanguageDirectives" $ do
    it "parses sample language directives" $ do
      let emptyBashDirectives = Bash $ BashDirectives []
      parseS pLanguageDirectives "#!magix bash\n#!packages bar" $ Bash (BashDirectives ["bar"])
      parseS pLanguageDirectives "#!magix bash" emptyBashDirectives
      parseS pLanguageDirectives "#!magix bash\t" emptyBashDirectives
      parseS pLanguageDirectives "#!magix \tbash\t" emptyBashDirectives
      parseS pLanguageDirectives "#!magix \tbash\n\n" emptyBashDirectives

    it "fails on wrong language directives" $ do
      parseF pLanguageDirectives "#! bar"
      parseF pLanguageDirectives "#!magix bash\n\n#!packages bar"
      parseF pLanguageDirectives "#!magix bash #!packages bar"
      parseF pLanguageDirectives "#! magix bash\n#!packages bar"
      parseF pLanguageDirectives "#!magix bash\n\n#!"
      parseF pLanguageDirectives "#!magix bash\n\n#!foo"

  describe "pDirectives" $ do
    it "parses minimal sample scripts" $ do
      minimalBash <- readMinimalBash
      parse pDirectives fnMinimalBash minimalBash
        `shouldBe` Right (Bash (BashDirectives ["jq"]))
      minimalHaskell <- readMinimalHaskell
      parse pDirectives fnMinimalHaskell minimalHaskell
        `shouldBe` Right (Haskell (HaskellDirectives ["bytestring"] ["-threaded"]))

    it "parses some edge cases" $ do
      let spaceTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                "#!packages a ",
                ""
              ]
      parseS pDirectives spaceTest $ Bash (BashDirectives ["a"])
      let newlineTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                "#!packages a",
                ""
              ]
      parseS pDirectives newlineTest $ Bash (BashDirectives ["a"])
      let newlineEmptyTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                ""
              ]
      parseS pDirectives newlineEmptyTest $ Bash (BashDirectives [])
      let emptySpaceTest :: Text =
            unlines
              [ "#!/usr/bin/env \t magix\t ",
                "#!magix \t bash \t"
              ]
      parseS pDirectives emptySpaceTest $ Bash (BashDirectives [])

    it "fails on some edge cases" $
      let directiveNotNewlineTest :: Text =
            unlines
              ["#!/usr/bin/env \t magix\t #!magix \t bash \t"]
       in parseF pDirectives directiveNotNewlineTest
