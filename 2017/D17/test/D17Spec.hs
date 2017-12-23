module D17Spec where

import Test.Hspec
import Test.QuickCheck

import D17Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "step" $ do
    it "step" $ do
      let s = step 3
      let b0 = new
      let b1 = s b0
      b1 `shouldBe` Buffer [0, 1] 1
      let b2 = s b1
      b2 `shouldBe` Buffer [0, 2, 1] 1
      let b3 = s b2
      b3 `shouldBe` Buffer [0, 2, 3, 1] 2

  describe "after" $ do
    it "finds the right thing for the sample for p1" $ do
      let b = run 2017 3
      bAfter 2017 b `shouldBe` 638

  describe "run2" $ do
    it "gets the right answer for p1 sample" $ do
      let (_, _, at1) = run2 9 3
      at1 `shouldBe` 9
