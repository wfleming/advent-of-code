module D15Spec where

import Test.Hspec
import Test.QuickCheck

import D15Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "next" $ do
    it "iterates sample generator a" $ do
      let ga0 = Gen { gSeed = 65, gFactor = factorA }
      let (ga1, v1) = next ga0
      v1 `shouldBe` 1092455
      let (ga2, v2) = next ga1
      v2 `shouldBe` 1181022009
      let (ga3, v3) = next ga2
      v3 `shouldBe` 245556042
      let (ga4, v4) = next ga3
      v4 `shouldBe` 1744312007
      let (ga5, v5) = next ga4
      v5 `shouldBe` 1352636452

    it "iterates sample generator b" $ do
      let gb0 = Gen { gSeed = 8921, gFactor = factorB }
      let (gb1, v1) = next gb0
      v1 `shouldBe` 430625591
      let (gb2, v2) = next gb1
      v2 `shouldBe` 1233683848
      let (gb3, v3) = next gb2
      v3 `shouldBe` 1431495498
      let (gb4, v4) = next gb3
      v4 `shouldBe` 137874439
      let (gb5, v5) = next gb4
      v5 `shouldBe` 285222916

  describe "lpad" $ do
    it "pads a string to specified length" $ do
      lpad 6 '0' "101" `shouldBe` "000101"

  describe "matches" $ do
    it "does not match numbers that shouldn't" $ do
      matches 1092455 430625591 `shouldBe` False
      matches 1181022009 1233683848 `shouldBe` False

    it "matches numbers that should" $ do
      matches 245556042 1431495498 `shouldBe` True

  describe "runJudge" $ do
    it "counts the correct number of matches for sample" $ do
      let ga = Gen { gSeed = 65, gFactor = factorA }
      let gb = Gen { gSeed = 8921, gFactor = factorB }
      runJudge ga gb 5 `shouldBe` 1
