module D15Spec (main, spec) where

import Test.Hspec

import D15Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "tickBy" $
        it "moves a disc" $ do
            let d = Disc { count = 5, pos = 4 }
            tickBy 1 d `shouldBe` Disc { count = 5, pos = 0 }
            tickBy 2 d `shouldBe` Disc { count = 5, pos = 1 }
            tickBy 5 d `shouldBe` Disc { count = 5, pos = 4 }

    describe "pass" $ do
        it "does not pass" $
            pass t0 `shouldBe` False

        it "passes" $
            pass (map (tickBy 5) t0) `shouldBe` True

    describe "firstPass" $
        it "finds an appropriate time" $
            firstPass t0 `shouldBe` 5

t0 = [Disc { count = 5, pos = 4 }, Disc { count = 2, pos = 1 }]
