module D4Spec (main, spec) where

import Test.Hspec

import qualified D4Lib as L
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "valid" $
        it "knows if a room is valid" $ do
            L.valid (L.Room "aaaaa-bbb-z-y-x" 0 "abxyz") `shouldBe` True
            L.valid (L.Room "a-b-c-d-e-f-g-h" 0 "abcde") `shouldBe` True
            L.valid (L.Room "not-a-real-room" 0 "oarel") `shouldBe` True
            L.valid (L.Room "totally-a-real-room" 0 "decoy") `shouldBe` False

    describe "calcChecksum" $
        it "calculates a checksum" $
            L.calcChecksum "aaaaa-bbb-z-y-x" `shouldBe` "abxyz"

    describe "decrypt" $
        it "decrypts a string" $
            L.decryptName "qzmt-zixmtkozy-ivhz" 343 `shouldBe` "very encrypted name"

    describe "parse" $
        it "parses a string" $ do
            let r = P.parse L.parser "fixture" "name-of-room-123[check]\nother-room-name-01[sumxz]\n"
            r `shouldBe` Right
                [ L.Room "name-of-room-" 123 "check"
                , L.Room "other-room-name-" 1 "sumxz"
                ]

