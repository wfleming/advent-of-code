module D8Lib.ParserSpec where

import Test.Hspec

import Text.Megaparsec (parse, ParseError, Dec)
import D8Lib.Data
import D8Lib.Parser

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "action" $ do
    it "parses an inc with positive x" $
      parse action "NA" "inc 10" `shouldBe` Right (Inc 10)

    it "parses an inc with negative x" $
      parse action "NA" "inc -10" `shouldBe` Right (Inc (-10))

    it "parses a dec with positive x" $
      parse action "NA" "dec 10" `shouldBe` Right (Dec 10)

    it "parses an dec with negative x" $
      parse action "NA" "dec -10" `shouldBe` Right (Dec (-10))

  describe "comparison" $ do
    it "parses a comparison.1" $ do
      parse comparison "NA" "b > 1" `shouldBe` Right (Cmp "b" Gt 1)

    it "parses a comparison.2" $ do
      parse comparison "NA" "a >= 1" `shouldBe` Right (Cmp "a" Gte 1)

  describe "rule" $ do
    it "parses a rule" $ do
      let r = parse rule "NO_INPUT_FILE" "a inc 42 if b > 1\n"
      r `shouldBe` Right (Rule "a" (Inc 42) (Cmp "b" Gt 1))

    it "parses a rule.2" $ do
      let r = parse rule "NO_INPUT_FILE" "c dec -10 if a >= 1\n"
      r `shouldBe` Right (Rule "c" (Dec (-10)) (Cmp "a" Gte 1))

    it "parses a rule.3" $ do
      let r = parse rule "NO_INPUT_FILE" "cpv inc 669 if csu >= -6\n"
      r `shouldBe` Right (Rule "cpv" (Inc 669) (Cmp "csu" Gte (-6)))

  describe "rules" $ do
    it "parses multiple rules" $ do
      let r = parse rules "NO_INPUT_FILE" "a inc 42 if b > 1\nb dec 10 if a == 0\n"
      r `shouldBe` Right
            [ Rule "a" (Inc 42) (Cmp "b" Gt 1)
            , Rule "b" (Dec 10) (Cmp "a" Eq 0)
            ]
