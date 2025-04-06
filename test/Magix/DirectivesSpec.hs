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

import Data.Either (isLeft, isRight)
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
import Magix.Tools (parse')
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
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
      parse pShebang "" "#!/usr/bin/env magix" `shouldSatisfy` isRight

    it "fails on wrong shebangs" $ do
      parse pShebang "" " #!/usr/bin/env magix" `shouldSatisfy` isLeft
      parse pShebang "" "#! /usr/bin/env magix" `shouldSatisfy` isLeft
      parse pShebang "" "#!/usr/bin/env magis" `shouldSatisfy` isLeft
      parse pShebang "" "#!/usr/bin/env" `shouldSatisfy` isLeft
      parse pShebang "" "#/usr/bin/env magix" `shouldSatisfy` isLeft
      parse pShebang "" "#/usr/bin/env3magix" `shouldSatisfy` isLeft

  describe "pMagixDirective" $ do
    it "parses sample Magix directives" $ do
      parse pMagixDirective "" "#!magix haskell" `shouldBe` Right "haskell"
      parse pMagixDirective "" "#!magix foo" `shouldBe` Right "foo"

    it "fails on wrong Magix directives" $ do
      parse pMagixDirective "" "#!magic haskell" `shouldSatisfy` isLeft
      parse pMagixDirective "" "#!magic" `shouldSatisfy` isLeft

  describe "pLanguageDirectives" $ do
    it "parses sample language directives" $ do
      parse pLanguageDirectives "" "#!magix bash\n#!packages bar" `shouldSatisfy` isRight
      parse pLanguageDirectives "" "#!magix bash" `shouldSatisfy` isRight
      parse pLanguageDirectives "" "#!magix bash\t" `shouldSatisfy` isRight
      parse pLanguageDirectives "" "#!magix \tbash\t" `shouldSatisfy` isRight
      parse pLanguageDirectives "" "#!magix \tbash\n\n" `shouldSatisfy` isRight

    it "fails on wrong language directives" $ do
      parse pLanguageDirectives "" "#! bar" `shouldSatisfy` isLeft
      parse pLanguageDirectives "" "#!magix bash\n\n#!packages bar" `shouldSatisfy` isLeft
      parse pLanguageDirectives "" "#!magix bash #!packages bar" `shouldSatisfy` isLeft
      parse pLanguageDirectives "" "#! magix bash\n#!packages bar" `shouldSatisfy` isLeft
      parse pLanguageDirectives "" "#!magix bash\n\n#!" `shouldSatisfy` isLeft
      parse pLanguageDirectives "" "#!magix bash\n\n#!foo" `shouldSatisfy` isLeft

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
      parse' pDirectives spaceTest `shouldBe` Bash (BashDirectives ["a"])
      let newlineTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                "#!packages a",
                ""
              ]
      parse' pDirectives newlineTest `shouldBe` Bash (BashDirectives ["a"])
      let newlineEmptyTest :: Text =
            unlines
              [ "#!/usr/bin/env magix",
                "#!magix bash",
                ""
              ]
      parse' pDirectives newlineEmptyTest `shouldBe` Bash (BashDirectives [])
      let emptySpaceTest :: Text =
            unlines
              [ "#!/usr/bin/env \t magix\t ",
                "#!magix \t bash \t"
              ]
      parse' pDirectives emptySpaceTest `shouldBe` Bash (BashDirectives [])

    it "fails on some edge cases" $
      let directiveNotNewlineTest :: Text =
            unlines
              ["#!/usr/bin/env \t magix\t #!magix \t bash \t"]
       in parse pDirectives "" directiveNotNewlineTest `shouldSatisfy` isLeft
