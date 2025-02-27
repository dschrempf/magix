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
import Magix.Directives (Directives (..), pDirectives, pShebang)
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

spaceTest :: Text
spaceTest =
  unlines
    [ "#!/usr/bin/env magix",
      "#!magix bash",
      "#!packages a ",
      ""
    ]

newlineTest :: Text
newlineTest =
  unlines
    [ "#!/usr/bin/env magix",
      "#!magix bash",
      "#!packages a",
      ""
    ]

newlineEmptyTest :: Text
newlineEmptyTest =
  unlines
    [ "#!/usr/bin/env magix",
      "#!magix bash",
      ""
    ]

emptySpaceTest :: Text
emptySpaceTest =
  unlines
    [ "#!/usr/bin/env \t magix\t ",
      "#!magix \t bash \t"
    ]

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

  describe "pDirectives" $ do
    it "parses minimal sample scripts" $ do
      minimalBash <- readMinimalBash
      parse pDirectives fnMinimalBash minimalBash
        `shouldBe` Right (Bash (BashDirectives ["jq"]))
      minimalHaskell <- readMinimalHaskell
      parse pDirectives fnMinimalHaskell minimalHaskell
        `shouldBe` Right (Haskell (HaskellDirectives ["bytestring"] ["-threaded"]))

    it "parses some edge cases" $ do
      parse' pDirectives spaceTest `shouldBe` Bash (BashDirectives ["a"])
      parse' pDirectives newlineTest `shouldBe` Bash (BashDirectives ["a"])
      parse' pDirectives newlineEmptyTest `shouldBe` Bash (BashDirectives [])
      parse' pDirectives emptySpaceTest `shouldBe` Bash (BashDirectives [])
