-- |
-- Module      :  Magix.NixpkgsPathPropertiesSpec
-- Description :  Properties of the NIX_PATH parser
-- Copyright   :  2026 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Tue Nov 17 11:05:11 2026.
--
-- The value of @NIX_PATH@ is a sequence of entries separated by runs of colons
-- and white space. An entry is split at its first @=@ into a prefix and a
-- target. A colon joins the text after it to the entry if the target so far (the
-- text after the last @=@) is @flake@ or @channel@, or a URL scheme followed by
-- @//@.
module Magix.NixpkgsPathPropertiesSpec
  ( spec,
  )
where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Magix.NixpkgsPath
  ( NixPathEntry (..),
    NixPathTarget (..),
    getDefaultNixpkgsPath,
    renderNixPathEntry,
  )
import Test.Hspec (Spec, describe, it)
import Test.Hspec.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck
  ( Gen,
    Property,
    arbitraryUnicodeChar,
    conjoin,
    counterexample,
    elements,
    forAllShrink,
    frequency,
    listOf,
    listOf1,
    oneof,
    scale,
    shrinkList,
    suchThat,
    (===),
  )

isSepChar :: Char -> Bool
isSepChar c = c == ':' || isSpace c

isEntryChar :: Char -> Bool
isEntryChar = not . isSepChar

spaceChars :: [Char]
spaceChars = filter isSpace [minBound .. maxBound]

-- | Targets before which a colon may join the next text to the entry (a URL
-- scheme only before @//@).
keywords :: [String]
keywords = ["flake", "channel", "http", "https", "file", "git", "s3", "ssh"]

urlSchemes :: [String]
urlSchemes = filter (`notElem` ["flake", "channel"]) keywords

parseNixPath :: String -> Either String [NixPathEntry]
parseNixPath = either (Left . show) Right . getDefaultNixpkgsPath

-- | Text of entry characters, path-like, sometimes holding @=@ or a keyword.
genText :: Gen String
genText =
  frequency
    [ (6, scale (min 12) (listOf genEntryChar)),
      (1, elements keywords),
      (1, (<>) <$> genText <*> ((<>) "=" <$> elements keywords))
    ]
  where
    genEntryChar =
      frequency
        [ (8, elements $ ['a' .. 'z'] <> ['0' .. '9'] <> "/.-_+"),
          (1, pure '='),
          (1, arbitraryUnicodeChar `suchThat` isEntryChar)
        ]

genTextWithout :: Char -> Gen String
genTextWithout c = filter (/= c) <$> genText

-- | A target, its text from the given generator.
genTarget :: Gen String -> Gen NixPathTarget
genTarget genT =
  oneof
    [ TargetPath <$> genT,
      TargetFlake <$> genT,
      TargetRemote . ("channel:" <>) <$> genT,
      TargetRemote <$> ((\s r -> s <> "://" <> r) <$> elements urlSchemes <*> genT)
    ]

-- | An unprefixed entry holds no @=@, which Nix reads as prefixed, and is not
-- empty.
genEntry :: Gen NixPathEntry
genEntry =
  oneof
    [ Prefixed <$> genTextWithout '=' <*> genTarget genText,
      Unprefixed <$> genTarget (genTextWithout '=') `suchThat` (/= TargetPath "")
    ]

-- | Whether a colon after the entry joins the given text after the colon to
-- the entry.
joinsColon :: NixPathEntry -> String -> Bool
joinsColon e after = case e of
  Prefixed _ (TargetPath p) -> joins $ reverse $ takeWhile (/= '=') $ reverse p
  Unprefixed (TargetPath p) -> joins p
  _ -> False
  where
    joins t = t `elem` ["flake", "channel"] || (t `elem` urlSchemes && "//" `isPrefixOf` after)

genSepChar :: Gen Char
genSepChar = frequency [(4, pure ':'), (2, pure ' '), (1, elements "\t\n"), (1, elements spaceChars)]

genSep :: Gen String
genSep = scale (min 4) (listOf1 genSepChar)

-- | A value of @NIX_PATH@.
data NixPath = NixPath
  { leading :: !String,
    -- | Each entry with the separator after it, which may be empty after the
    -- last entry.
    entries :: ![(NixPathEntry, String)]
  }
  deriving (Show)

render :: NixPath -> String
render x = leading x <> concat [renderNixPathEntry e <> s | (e, s) <- entries x]

-- | Start a separator with a space where its colon would join the next entry.
separate :: NixPath -> NixPath
separate x = x {entries = zipWith fix (entries x) nexts}
  where
    nexts = drop 1 (map (renderNixPathEntry . fst) (entries x)) <> [""]
    fix (e, s) n
      | take 1 s == ":" && joinsColon e (drop 1 s <> n) = (e, ' ' : s)
      | otherwise = (e, s)

genNixPath :: Gen NixPath
genNixPath = do
  l <- oneof [pure "", genSep]
  es <- scale (min 8) (listOf genEntry)
  seps <- traverse (const genSep) es
  lastSep <- oneof [pure "", genSep]
  let seps' = if null seps then [] else init seps <> [lastSep]
  pure $ separate $ NixPath l (zip es seps')

-- | Drop entries, and shorten the separators to one character.
shrinkNixPath :: NixPath -> [NixPath]
shrinkNixPath x =
  map separate $
    [x {leading = ""} | not (null (leading x))]
      <> [x {entries = es} | es <- shrinkList (const []) (entries x)]
      <> [x {entries = es} | let es = map shortSep (entries x), map snd es /= map snd (entries x)]
  where
    shortSep (e, s) = (e, take 1 s)

-- | Arbitrary text, biased toward the grammar.
genInput :: Gen String
genInput =
  concat
    <$> scale
      (min 16)
      ( listOf $
          frequency
            [ (4, pure <$> arbitraryUnicodeChar),
              (4, pure <$> elements "abc/=.:"),
              (2, pure <$> genSepChar),
              (1, elements keywords),
              (1, pure "//")
            ]
      )

shrinkString :: String -> [String]
shrinkString = shrinkList (const [])

propParsesBack :: Property
propParsesBack =
  forAllShrink genNixPath shrinkNixPath $ \x ->
    counterexample (show $ render x) $
      parseNixPath (render x) === Right (map fst (entries x))

-- | Every character is either a separator or part of an entry, so every value
-- parses. Its entries are not empty, and hold its entry characters in order.
propTotal :: Property
propTotal =
  forAllShrink genInput shrinkString $ \s ->
    counterexample (show s) $ case parseNixPath s of
      Left err -> counterexample err False
      Right es ->
        let rs = map renderNixPathEntry es
         in conjoin
              [ counterexample "empty entry" $ notElem "" rs,
                counterexample "white space in an entry" $ not (any (any isSpace) rs),
                filter isEntryChar (concat rs) === filter isEntryChar s
              ]

-- | White space ends an entry.
data Split = Split
  { before :: !String,
    space :: !String,
    after :: !String
  }
  deriving (Show)

joinSplit :: Split -> String
joinSplit x = before x <> space x <> after x

-- | The entries of two values joined by white space are the entries of the
-- first, then those of the second.
propSpaceSplits :: Property
propSpaceSplits =
  forAllShrink (Split <$> genInput <*> genSpace <*> genInput) shrinkSplit $ \x ->
    counterexample (show $ joinSplit x) $
      parseNixPath (joinSplit x) === ((<>) <$> parseNixPath (before x) <*> parseNixPath (after x))
  where
    genSpace = scale (min 4) (listOf1 (elements spaceChars))
    shrinkSplit x =
      [x {before = a} | a <- shrinkString (before x)]
        <> [x {after = b} | b <- shrinkString (after x)]
        <> [x {space = " "} | space x /= " "]

propSeparatorsOnly :: Property
propSeparatorsOnly =
  forAllShrink (scale (min 16) (listOf genSepChar)) shrinkString $ \s ->
    counterexample (show s) $ parseNixPath s === Right []

spec :: Spec
spec =
  modifyMaxSuccess (const 1000) $
    describe "getDefaultNixpkgsPath" $ do
      it "parses rendered entries back, in order" propParsesBack
      it "parses any value, keeping its entry characters in order" propTotal
      it "splits at white space" propSpaceSplits
      it "parses separators alone as no entries" propSeparatorsOnly
