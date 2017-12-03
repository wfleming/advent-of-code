module D1Spec where

import Test.Hspec
import Test.QuickCheck

import D1Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseStr" $ do
    it "parses a series of digits" $ do
      let r = parseStr "12345"
      r `shouldBe` Right [1, 2, 3, 4, 5]

  describe "calc1" $ do
    it "sums 1122" $ do
      calc1 [1, 1, 2, 2] `shouldBe` 3

    it "sums 1111" $ do
      calc1 [1, 1, 1, 1] `shouldBe` 4

    it "sums 1234" $ do
      calc1 [1, 2, 3, 4] `shouldBe` 0

    it "sums 91212129 " $ do
      calc1 [9, 1, 2, 1, 2, 1, 2, 9] `shouldBe` 9

  describe "calc2" $ do
    it "sums 1212" $ do
      calc2 [1, 2, 1, 2] `shouldBe` 6

    it "sums 1221" $ do
      calc2 [1, 2, 2, 1] `shouldBe` 0

    it "sums 123425" $ do
      calc2 [1, 2, 3, 4, 2, 5] `shouldBe` 4

    it "sums 123123" $ do
      calc2 [1, 2, 3, 1, 2, 3] `shouldBe` 12

    it "sums 12131415" $ do
      calc2 [1, 2, 1, 3, 1, 4, 1, 5] `shouldBe` 4
