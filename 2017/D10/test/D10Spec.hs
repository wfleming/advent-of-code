module D10Spec where

import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec (parse, ParseError, Dec)
import D10Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
    it "parses a list of ints" $ do
      let r = parse parser "NO_INPUT_FILE" "123,456,78,9\n"
      r `shouldBe` Right [123, 456, 78, 9]

  describe "step" $ do
    it "steps a small example correctly" $ do
      let s0 = new [0..4] [3, 4, 1, 5]
      s0 `shouldBe` State [0, 1, 2, 3, 4] [3, 4, 1, 5] 0 0

      let s1 = step s0
      s1 `shouldBe` State [2, 1, 0, 3, 4] [4, 1, 5] 3 1

      let s2 = step s1
      s2 `shouldBe` State [4, 3, 0, 1, 2] [1, 5] 3 2

      let s3 = step s2
      s3 `shouldBe` State [4, 3, 0, 1, 2] [5] 1 3

      let s4 = step s3
      s4 `shouldBe` State [3, 4, 2, 1, 0] [] 4 4
