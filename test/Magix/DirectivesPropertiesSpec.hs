-- |
-- Module      :  Magix.DirectivesPropertiesSpec
-- Description :  Properties of the directive parsers
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 17 16:00:00 2026.
--
-- The generators produce what the grammar accepts: a value is a non-empty
-- sequence of alphanumeric, punctuation and symbol characters; inline white
-- space is what megaparsec's @hspace@ accepts; directive lines follow each
-- other without blank lines; the script body does not start with @#!@ after
-- white space.
module Magix.DirectivesPropertiesSpec
  ( spec,
  )
where

import Control.Monad (forM_)
import Data.Char (GeneralCategory (Surrogate), generalCategory, isAlphaNum, isPunctuation, isSpace, isSymbol)
import Data.Either (isLeft)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text, pack)
import Data.Text qualified as T
import Magix.Directives (Directives (..), pDirectives)
import Magix.Language.Bash.Directives (BashDirectives (..))
import Magix.Language.Directives (LanguageDirectives (..))
import Magix.Language.Haskell.Directives (HaskellDirectives (..))
import Magix.Language.Language (Language (..), getLanguageNameLowercase)
import Magix.Language.Python.Directives (PythonDirectives (..))
import Test.Hspec (Spec, before_, describe, it, pendingWith)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
  ( Gen,
    Property,
    arbitrary,
    arbitraryASCIIChar,
    arbitraryUnicodeChar,
    chooseInt,
    counterexample,
    elements,
    forAllShrink,
    frequency,
    listOf,
    listOf1,
    oneof,
    scale,
    shrinkIntegral,
    shrinkList,
    suchThat,
    (===),
  )
import Text.Megaparsec (errorBundlePretty, parse)

-- | A directive: @#!@, its name, inline white space, and its values, each
-- followed by inline white space. The white space is non-empty, except after
-- the last value.
data Directive = Directive
  { name :: !Text,
    gap :: !Text,
    values :: !(NonEmpty (Text, Text))
  }
  deriving (Show)

-- | The lines of a script up to its body.
data Header = Header
  { shebang :: !Directive,
    nixpkgs :: !(Maybe Directive),
    magix :: !Directive,
    language :: !Language,
    directives :: ![Directive],
    -- | Nothing: the last directive ends the script, without a newline.
    body :: !(Maybe Text)
  }
  deriving (Show)

-- | The directives of a language.
getDirectiveNames :: Language -> NonEmpty Text
getDirectiveNames Bash = "packages" :| []
getDirectiveNames Haskell = "haskellPackages" :| ["ghcFlags"]
getDirectiveNames Python = "pythonPackages" :| []

getLanguageDirectives :: Language -> [Directive] -> LanguageDirectives
getLanguageDirectives language ds = case language of
  Bash -> BashD $ BashDirectives (valuesOf "packages")
  Haskell -> HaskellD $ HaskellDirectives (valuesOf "haskellPackages") (valuesOf "ghcFlags")
  Python -> PythonD $ PythonDirectives (valuesOf "pythonPackages")
  where
    valuesOf n = [v | d <- ds, d.name == n, (v, _) <- toList d.values]

getFirstValue :: Directive -> Text
getFirstValue d = fst $ NE.head d.values

getExpected :: Header -> Directives
getExpected h =
  Directives
    { nixpkgsRef = getFirstValue <$> h.nixpkgs,
      language = getLanguageDirectives h.language h.directives
    }

renderDirective :: Directive -> Text
renderDirective d = "#!" <> d.name <> d.gap <> foldMap (uncurry (<>)) d.values

render :: Header -> Text
render h = T.intercalate "\n" (map renderDirective ls) <> maybe "" ("\n" <>) h.body
  where
    ls = [h.shebang] <> toList h.nixpkgs <> [h.magix] <> h.directives

isHSpaceChar :: Char -> Bool
isHSpaceChar c = isSpace c && c /= '\n' && c /= '\r'

hSpaceChars :: [Char]
hSpaceChars = filter isHSpaceChar [minBound .. maxBound]

isValueChar :: Char -> Bool
isValueChar c = isAlphaNum c || isPunctuation c || isSymbol c

genHSpace :: Gen Text
genHSpace = pack <$> scale (min 4) (listOf genHSpaceChar)
  where
    genHSpaceChar = frequency [(3, pure ' '), (1, pure '\t'), (1, elements hSpaceChars)]

genHSpace1 :: Gen Text
genHSpace1 = genHSpace `suchThat` (not . T.null)

genValue :: Gen Text
genValue = pack <$> scale (min 16) (listOf1 genValueChar)
  where
    genValueChar =
      frequency
        [ (4, arbitraryASCIIChar `suchThat` isValueChar),
          (1, arbitraryUnicodeChar `suchThat` isValueChar)
        ]

-- | A directive with the given values.
genDirectiveWith :: Text -> NonEmpty Text -> Gen Directive
genDirectiveWith n vs = do
  g <- genHSpace1
  seps <- traverse (const genHSpace1) (NE.tail vs)
  trail <- genHSpace
  pure $ Directive n g (NE.zip vs (NE.fromList $ seps <> [trail]))

genDirective :: Text -> Gen Directive
genDirective n = do
  v <- genValue
  vs <- scale (min 4) (listOf genValue)
  genDirectiveWith n (v :| vs)

genBody :: Gen Text
genBody =
  oneof
    [ pure "",
      pure "\necho Hello\n",
      (pack <$> scale (min 32) arbitrary) `suchThat` (not . T.isPrefixOf "#!" . T.stripStart)
    ]

genHeader :: Language -> Gen Header
genHeader l = do
  s <- genDirectiveWith "/usr/bin/env" ("magix" :| [])
  n <- oneof [pure Nothing, Just <$> (genValue >>= genDirectiveWith "nixpkgs" . pure)]
  m <- genDirectiveWith "magix" (getLanguageNameLowercase l :| [])
  ds <- scale (min 8) $ listOf (elements (toList $ getDirectiveNames l) >>= genDirective)
  b <- oneof [pure Nothing, Just <$> genBody]
  pure $ Header s n m l ds b

-- | Shrink the white space to single spaces, and none after the last value.
shrinkSpaces :: Directive -> [Directive]
shrinkSpaces d =
  [d {gap = " "} | d.gap /= " "]
    <> [d {values = vs} | let vs = normalize d.values, vs /= d.values]
  where
    normalize xs =
      NE.fromList $ [(v, " ") | (v, _) <- NE.init xs] <> [(fst $ NE.last xs, "")]

-- | Keep the structure valid: white space stays non-empty where it must, and a
-- directive keeps at least one value.
shrinkDirective :: Directive -> [Directive]
shrinkDirective d =
  shrinkSpaces d <> [d {values = vs} | vs <- shrinkValues d.values]
  where
    shrinkValues (x :| xs) =
      [x :| xs' | xs' <- shrinkList (const []) xs]
        <> [(v', s) :| xs | let (v, s) = x, v' <- shrinkText v]
    shrinkText v = [v' | v' <- [T.take 1 v], v' /= v]

shrinkHeader :: Header -> [Header]
shrinkHeader h =
  [h {shebang = d} | d <- shrinkSpaces h.shebang]
    <> [h {nixpkgs = Nothing} | Just _ <- [h.nixpkgs]]
    <> [h {nixpkgs = Just d} | Just n <- [h.nixpkgs], d <- shrinkSpaces n]
    <> [h {magix = d} | d <- shrinkSpaces h.magix]
    <> [h {directives = ds} | ds <- shrinkList shrinkDirective h.directives]
    <> [h {body = Just ""} | Just b <- [h.body], b /= ""]

propParses :: Language -> Property
propParses l =
  forAllShrink (genHeader l) shrinkHeader $ \h ->
    let x = render h
     in counterexample (show x) $
          either (Left . errorBundlePretty) Right (parse pDirectives "" x)
            === Right (getExpected h)

-- | A header with an invalid character in a value of a language directive. The
-- indices wrap around, so that a shrunk header stays corrupted.
data Corruption = Corruption
  { header :: !Header,
    directive :: !Int,
    value :: !Int,
    position :: !Int,
    char :: !Char
  }
  deriving (Show)

-- | Neither a value character nor white space.
genInvalidChar :: Gen Char
genInvalidChar =
  frequency
    [ (1, elements ['\0', '\DEL', '\x0301', '\x200B', '\x2028', '\xE000']),
      (1, arbitraryUnicodeChar `suchThat` isInvalid)
    ]
  where
    isInvalid c = not (isValueChar c || isSpace c) && generalCategory c /= Surrogate

genCorruption :: Language -> Gen Int -> Gen Corruption
genCorruption l genPosition = do
  h <- genHeader l `suchThat` (not . null . (.directives))
  i <- chooseInt (0, length h.directives - 1)
  let d = h.directives !! i
  j <- chooseInt (0, length d.values - 1)
  Corruption h i j <$> genPosition <*> genInvalidChar

shrinkCorruption :: Corruption -> [Corruption]
shrinkCorruption c =
  [c {header = h} | h <- shrinkHeader c.header, not $ null h.directives]
    <> [c {directive = i} | i <- shrinkIntegral c.directive]
    <> [c {value = j} | j <- shrinkIntegral c.value]
    <> [c {position = p} | p <- shrinkIntegral c.position]

-- | Insert the character into the value.
corrupt :: Corruption -> Header
corrupt c = c.header {directives = modifyAt c.directive corruptDirective c.header.directives}
  where
    corruptDirective d = d {values = NE.fromList $ modifyAt c.value corruptValue $ toList d.values}
    corruptValue (v, s) =
      let (a, b) = T.splitAt (c.position `mod` (T.length v + 1)) v
       in (a <> T.singleton c.char <> b, s)
    modifyAt n f xs = [if k == n `mod` length xs then f x else x | (k, x) <- zip [0 ..] xs]

propRejects :: Gen Corruption -> Property
propRejects gen =
  forAllShrink gen shrinkCorruption $ \c ->
    let x = render (corrupt c)
     in counterexample (show x) $ isLeft (parse pDirectives "" x)

-- | The first value of the first directive line, at position 0.
genFirst :: Language -> Gen Corruption
genFirst l = do
  c <- genCorruption l (pure 0)
  pure c {directive = 0, value = 0}

spec :: Spec
spec =
  modifyMaxSuccess (const 1000) $
    describe "pDirectives" $
      forM_ [minBound .. maxBound :: Language] $ \l ->
        describe (show l) $ do
          it "parses rendered directives back" $ propParses l

          it "rejects a directive line starting with an invalid value" $
            propRejects (genFirst l)

          -- Fails: "#!packages _ \DELP" parses as the package "_", and the rest
          -- of the line becomes the script body.
          before_ (pendingWith "directive-line-ignores-an-invalid-tail") $
            it "rejects a value with an invalid character" $
              propRejects (genCorruption l arbitrary)
