module D16Lib.ParserSpec where

import Test.Hspec

import D16Lib (Step(..))
import D16Lib.Parser
import Text.Megaparsec (parse)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "step" $ do
    it "parses a spin" $ do
      parse step "NA" "s12" `shouldBe` Right (Spin 12)

    it "parses an exchange" $ do
      parse step "NA" "x9/13" `shouldBe` Right (Exchange 9 13)

    it "parses a partner" $ do
      parse step "NA" "pa/d" `shouldBe` Right (Partner 'a' 'd')

  describe "steps" $ do
    it "parses several steps" $ do
      let ss = parse steps "NA" "x1/2,s3,pa/b\n"
      ss `shouldBe` Right [Exchange 1 2, Spin 3, Partner 'a' 'b']
