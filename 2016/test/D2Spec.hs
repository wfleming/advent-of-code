module D2Spec (main, spec) where

import Test.Hspec

import D2Lib
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "move" $ do
        it "moves up" $ do
            move 4 U `shouldBe` 1
            move 1 U `shouldBe` 1
            move 3 U `shouldBe` 3

        it "moves down" $ do
            move 4 D `shouldBe` 7
            move 1 D `shouldBe` 4
            move 7 D `shouldBe` 7
            move 9 D `shouldBe` 9

        it "moves left" $ do
            move 1 L `shouldBe` 1
            move 2 L `shouldBe` 1
            move 4 L `shouldBe` 4
            move 9 L `shouldBe` 8

        it "moves right" $ do
            move 1 R `shouldBe` 2
            move 3 R `shouldBe` 3
            move 4 R `shouldBe` 5
            move 9 R `shouldBe` 9

    describe "parse" $
        it "parses a string" $ do
            let r = P.parse parser "fixture" "UDD\nLRUD"
            r `shouldBe` Right [[U, D, D], [L, R, U, D]]

    describe "key" $
        it "returns where a series of movements stops" $ do
            key 5 [U, U] `shouldBe` 2
            key 5 [U, D] `shouldBe` 5
            key 5 [U, D, R, R] `shouldBe` 6
            key 1 [U, R, D, L] `shouldBe` 4

    describe "code" $
        it "returns a series of keys" $
            code 5 [[U, L], [R, R, D, D], []] `shouldBe` [1, 9]

    describe "move'" $
        it "moves correctly" $ do
          move' 1 D `shouldBe` 3
          move' 5 U `shouldBe` 5
          move' 5 L `shouldBe` 5
          move' 5 R `shouldBe` 6
          move' 8 U `shouldBe` 4
          move' 9 U `shouldBe` 9
