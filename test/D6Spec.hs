module D6Spec (main, spec) where

import Test.Hspec

import D6Lib
import Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "transpose" $
        it "does what I think it does" $ do
            let input = ["abc", "def", "ghi"]
            transpose input `shouldBe` ["adg", "beh", "cfi"]

    describe "fixMessage" $
        it "works" $ do
            let input = ["cbt", "cab", "kat", "xss"]
            fixMessage input `shouldBe` "cat"

    describe "fixModMessage" $
        it "works" $ do
            let input = ["cbt", "cab", "kat", "kxs", "xss"]
            fixModMessage input `shouldBe` "xbb"
