module D9Spec (main, spec) where

import Test.Hspec

import D9Lib
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "decompressV1" $
        it "decompresses messages" $ do
            let p = P.parse decompressV1 "fixture"
            p "ADVENT" `shouldBe` Right "ADVENT"
            p "A(1x5)BC" `shouldBe` Right "ABBBBBC"
            p "(3x3)XYZ" `shouldBe` Right "XYZXYZXYZ"
            p "A(2x2)BCD(2x2)EFG" `shouldBe` Right "ABCBCDEFEFG"
            p "(6x1)(1x3)A" `shouldBe` Right "(1x3)A"
            p "X(8x2)(3x3)ABCY" `shouldBe` Right "X(3x3)ABC(3x3)ABCY"

    describe "stripAll" $
        it "removes all whitespace" $ do
          stripAll "a \nc\n" `shouldBe` "ac"
          stripAll "a cccc\n\n\r" `shouldBe` "acccc"


    describe "decompressV2" $
        it "decompresses messages" $ do
            let p = P.parse decompressV2 "fixture"
            p "ADVENT" `shouldBe` Right (length "ADVENT")
            p "A(1x5)BC" `shouldBe` Right (length "ABBBBBC")
            p "(3x3)XYZ" `shouldBe` Right (length "XYZXYZXYZ")
            p "X(8x2)(3x3)ABCY" `shouldBe` Right (length "XABCABCABCABCABCABCY")
            p "(27x12)(20x12)(13x14)(7x10)(1x12)A" `shouldBe` Right 241920
            p "(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" `shouldBe` Right 445

            -- don't count whitespace
            p "AD VENT" `shouldBe` Right (length "ADVENT")
            p "A(2x5) BC" `shouldBe` Right (length "ABBBBBC")
