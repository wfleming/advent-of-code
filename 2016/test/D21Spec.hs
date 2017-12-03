module D21Spec (main, spec) where

import Test.Hspec

import D21Lib
import Data.Either (isRight)
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "parsing" $
        it "parses instructions" $ do
            let is = P.parse instructionsP "fixture" instructionsTextFixture
            is `shouldSatisfy` isRight

    describe "applying instructions" $
        it "changes strings" $ do
            apply "cat" (SwapPos 1 2) `shouldBe` "cta"
            apply "cat" (RotateDir R 1) `shouldBe` "tca"
            apply "cat" (RotateDir L 1) `shouldBe` "atc"
            apply "cat" (RotatePos 'a') `shouldBe` "atc"
            apply "cat" (Move 1 2) `shouldBe` "cta"
            apply "abcd" (Reverse 1 2) `shouldBe` "acbd"
            apply "abcde" (SwapLet 'd' 'b') `shouldBe` "adcbe"
            apply "ecabd" (RotatePos 'd') `shouldBe` "decab"

    describe "replaceAt" $
        it "replaces at the desired pos" $
            replaceAt "cat" 2 'r' `shouldBe` "car"

    describe "insertAt" $
        it "inserts without replacing" $
            insertAt "cat" 2 'r' `shouldBe` "cart"

    describe "end to end" $ do
        it "scrambles" $ do
            let instrs = either (error . show) id $ P.parse instructionsP "fixture" instructionsTextFixture
            let scrambled = scramble "abcde" instrs
            scrambled `shouldBe` "decab"

        it "unscrambles" $ do
            let instrs = either (error . show) id $ P.parse instructionsP "fixture" instructionsTextFixture
            let unscrambled = unscramble "decab" instrs
            unscrambled `shouldBe` "abcde"


instructionsTextFixture =
    "swap position 4 with position 0\n" ++
    "swap letter d with letter b\n" ++
    "reverse positions 0 through 4\n" ++
    "rotate left 1 step\n" ++
    "move position 1 to position 4\n" ++
    "move position 3 to position 0\n" ++
    "rotate based on position of letter b\n" ++
    "rotate based on position of letter d\n"

