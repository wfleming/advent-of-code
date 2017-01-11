module D19Spec (main, spec) where

import Test.Hspec

import D19Lib
import qualified Data.Sequence as S

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "lastStanding" $ do
        it "runs until one elf standing" $ do
            lastStanding [1..5] `shouldBe` 3

    describe "lastStanding2" $ do
        it "runs until one elf standing" $ do
            lastStanding2 [1..5] `shouldBe` 2

        it "calculates correct across idx for vector" $ do
            acrossIdx (S.fromList [1..5]) 0 `shouldBe` 2
            acrossIdx (S.fromList [1..5]) 1 `shouldBe` 3
            acrossIdx (S.fromList [1..4]) 0 `shouldBe` 2
            acrossIdx (S.fromList [1..2]) 1 `shouldBe` 0
