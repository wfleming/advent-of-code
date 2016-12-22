module D12Spec (main, spec) where

import Test.Hspec

import D12Lib
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "instructionsP" $
        it "parses messages" $ do
            let tape = P.parse instructionsP "fixture" "cpy -1 a\ninc a\ncpy a b\n"
            tape `shouldBe` Right
                [ Cpy (Right (-1)) (Reg 'a')
                , Inc (Reg 'a')
                , Cpy (Left $ Reg 'a') (Reg 'b')
                ]

    describe "terminated" $ do
        it "is terminated when pos is past end of tape" $ do
            let vm = VM { registers = [], tape = [Inc (Reg 'a')], pos = 1 }
            terminated vm `shouldBe` True

        it "is not terminated when pos is within tape range" $ do
            let vm = VM { registers = [], tape = [Inc (Reg 'a')], pos = 0 }
            terminated vm `shouldBe` False

    describe "run" $ do
        it "runs a calculation" $ do
            let egInput = "cpy 41 a\ninc a\ninc a\ndec a\njnz a 2\ndec a\n"
            let pRes = P.parse instructionsP "fixture" egInput
            let tape = either (error . show) id pRes
            let vm = newVM tape
            let vm' = run vm
            registers vm' `shouldBe` [('a', 42), ('b', 0), ('c', 0), ('d', 0)]
            pos vm' `shouldBe` 6


