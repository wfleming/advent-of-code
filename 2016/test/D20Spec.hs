module D20Spec (main, spec) where

import Test.Hspec

import D20Lib
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "parsing" $
        it "parses list of blockrules" $ do
            let s = "5-10\n2-3\n"
            let rs = P.parse blockListP "fixture" s
            rs `shouldBe` Right [BlockRule 5 10, BlockRule 2 3]

    describe "blocked" $
        it "determines if an ip is blocked" $ do
            let rs = [BlockRule 5 10, BlockRule 2 3]
            blocked rs 1 `shouldBe` False
            blocked rs 4 `shouldBe` False
            blocked rs 2 `shouldBe` True
            blocked rs 9 `shouldBe` True

    describe "simplifying block list" $ do
        it "combines rules" $ do
            let rs = [BlockRule 5 10, BlockRule 2 3, BlockRule 6 8, BlockRule 9 15]
            nubBlockList rs `shouldBe` [BlockRule 2 3, BlockRule 5 15]

        it "knows what's combinable" $ do
            let r1 = BlockRule 5 10
            let r2 = BlockRule 6 15
            combinable r1 r2 `shouldBe` True

        it "combines" $ do
            let r1 = BlockRule 5 10
            let r2 = BlockRule 6 15
            combine r1 r2 `shouldBe` BlockRule 5 15

    describe "lowestAllowed" $
        it "finds the lowest allowed IP" $ do
            let rs = [BlockRule 5 10, BlockRule 0 3]
            lowestAllowed rs `shouldBe` 4

    describe "allowedCount" $
        it "counts allowed IPs" $ do
            let rs = [BlockRule 5 10, BlockRule 2 3, BlockRule 6 8, BlockRule 9 15]
            -- all that's blocked is 2-3, 5-15. (and +1 since 0 is eligible)
            let c = ipIntMax + 1 - 2 - 11
            allowedCount rs `shouldBe` c
