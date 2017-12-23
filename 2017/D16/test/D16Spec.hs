module D16Spec where

import Test.Hspec
import Test.QuickCheck

import D16Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "step" $ do
    it "does a spin" $ do
      step "abcde" (Spin 2) `shouldBe` "deabc"

    it "does an exchange" $ do
      step "abcde" (Exchange 3 4) `shouldBe` "abced"
      step "abcde" (Exchange 0 4) `shouldBe` "ebcda"
      step "abcde" (Exchange 1 3) `shouldBe` "adcbe"

    it "does a partner swap" $ do
      step "abcde" (Partner 'a' 'e') `shouldBe` "ebcda"
      step "abcde" (Partner 'c' 'a') `shouldBe` "cbade"

  describe "run" $ do
    it "runs all steps" $ do
      let ds = run "abcde" [Spin 1, Exchange 3 4, Partner 'e' 'b']
      ds `shouldBe` "baedc"

  describe "cycleLength" $ do
    it "finds the cycle length of the steps" $ do
      let c = cycleLength "abcde" [Spin 1, Exchange 3 4, Partner 'e' 'b']
      c `shouldBe` 4
      let ds = runTimes "abcde" [Spin 1, Exchange 3 4, Partner 'e' 'b'] 4
      ds `shouldBe` "abcde"
