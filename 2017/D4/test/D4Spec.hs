module D4Spec where

import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec

import D4Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "passphraseParser" $ do
    it "parses a line" $ do
      let r = parse passphraseParser "NO_INPUT_FILE" "foo bar baz\n"
      r `shouldBe` Right ["foo", "bar", "baz"]

  describe "parseStr" $ do
    it "parses a series of lines" $ do
      let r = parse parser "NO_INPUT_FILE" "foo bar\nbaz buzz zzz\n"
      r `shouldBe` Right
          [ ["foo", "bar"]
          , ["baz", "buzz", "zzz"] ]

  describe "valid" $ do
    it "is True for passphrase with no dupes" $ do
      valid ["aaa", "bbb"] `shouldBe` True

    it "is False for passphrase with dupes" $ do
      valid ["aaa", "bbb", "aaa"] `shouldBe` False

  describe "validAnagramCount" $ do
    it "counts passphrases with no dupes" $ do
      validAnagramCount [["aaa", "bbb"]] `shouldBe` 1

    it "does not count passphrases with dupes" $ do
      validAnagramCount [["oiii", "ioii", "iioi"]] `shouldBe` 0
