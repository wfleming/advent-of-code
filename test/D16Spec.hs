module D16Spec (main, spec) where

import Test.Hspec

import D16Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "curve" $
        it "generates data" $ do
            let d = curve 23 "1"
            d `shouldBe` "10001100100111001000110"

    describe "curveStep" $
        it "steps" $ do
            curveStep "1" `shouldBe` "100"
            curveStep "0" `shouldBe` "001"
            curveStep "11111" `shouldBe` "11111000000"
            curveStep "111100001010" `shouldBe` "1111000010100101011110000"

    describe "checksum" $
        it "checksums" $
            checksum "110010110100" `shouldBe` "100"

    describe "end-to-end" $
        it "gets correct answers" $ do
            let d = curve 20 "10000"
            d `shouldBe` "10000011110010000111"
            checksum d `shouldBe` "01100"
