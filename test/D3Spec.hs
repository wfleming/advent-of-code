module D3Spec (main, spec) where

import Test.Hspec

import qualified D3Lib as L
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "valid" $
        it "knows if a triangle is valid" $ do
            L.valid (5, 10, 25) `shouldBe` False
            L.valid (3, 4, 5) `shouldBe` True
            L.valid (5, 3, 1) `shouldBe` False

    describe "parse" $
        it "parses a string" $ do
            let r = P.parse L.parser "fixture" "  1 2   3\n 44  567  3\n"
            r `shouldBe` Right [(1, 2, 3), (44, 567, 3)]


    describe "colTriangles" $
        it "transforms from rows to cols" $ do
            let r = L.colTriangles [ (1, 2, 3), (1, 2, 3), (1, 2, 3) , (4, 5, 6), (4, 5, 6), (4, 5, 6) , (7, 8, 9), (7, 8, 9), (7, 8, 9) ]
            r `shouldBe`
                [ (1, 1, 1), (4, 4, 4), (7, 7, 7)
                , (2, 2, 2), (5, 5, 5), (8, 8, 8)
                , (3, 3, 3), (6, 6, 6), (9, 9, 9)
                ]
