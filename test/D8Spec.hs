module D8Spec (main, spec) where

import Test.Hspec

import D8Lib
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "screen" $ do
        it "basic set/show functionality" $ do
            let s = screen 2 2
            show s `shouldBe` "..\n..\n"
            let s' = setPixel s On (1,1)
            show s' `shouldBe` "#.\n..\n"

        it "Matrix addressing" $ do
            let s = rect (screen 4 4) 3 2
            (s `getPixel` (1, 1)) `shouldBe` On
            (s `getPixel` (3, 1)) `shouldBe` On
            (s `getPixel` (1, 3)) `shouldBe` Off

            let s' = setPixel (screen 3 3) On (1, 2)
            show s' `shouldBe` ".#.\n...\n...\n"

        it "sets a rect to On" $ do
            let s = rect (screen 4 4) 3 2
            show s `shouldBe` "##..\n##..\n##..\n....\n"

        it "rotates a row" $ do
            let s = rotRow (rect (screen 4 4) 3 2) 2 2
            show s `shouldBe` "##..\n..##\n##..\n....\n"

        it "rotates a different  row" $ do
            let s = rect (screen 4 4) 2 3
            show s `shouldBe` "###.\n###.\n....\n....\n"
            let s' = rotRow s 1 2
            show s' `shouldBe` "#.##\n###.\n....\n....\n"

        it "rotates a col" $ do
            let s = rect (screen 4 4) 3 2
            show s `shouldBe` "##..\n##..\n##..\n....\n"
            let s' = rotCol s 1 2
            show s' `shouldBe` "##..\n.#..\n##..\n#...\n"

        it "counts lit pixels" $ do
            let s = rect (screen 4 4) 3 2
            litPixels s `shouldBe` 6

    describe "card" $ do
        it "parses rect instructions" $ do
            let input = "rect 3x2"
            let r = P.parse rectParser "fixture" input
            r `shouldBe` Right (Rect 3 2)

        it "parses rot row instructions" $ do
            let input = "rect 3x2"
            let input = "rotate row y=3 by 1"
            let r = P.parse rotRowParser "fixture" input
            r `shouldBe` Right (RotRow 4 1)

        it "parses rot col instructions" $ do
            let input = "rect 3x2"
            let input = "rotate column x=3 by 1"
            let r = P.parse rotColParser "fixture" input
            r `shouldBe` Right (RotCol 4 1)

        it "parses instructions" $ do
            let input = "rect 3x2\nrotate column x=1 by 1\nrotate row y=0 by 4\nrotate column x=1 by 1\n"
            let r = P.parse cardParser "fixture" input
            r `shouldBe` Right
              [ Rect 3 2
              , RotCol 2 1
              , RotRow 1 4
              , RotCol 2 1
              ]

        it "runs card instructions on a screen" $ do
            let s = screen 3 7
            let c = [Rect 3 2, RotCol 2 1, RotRow 1 4, RotCol 2 1]
            let s' = runCard c s
            show s' `shouldBe` ".#..#.#\n#.#....\n.#.....\n"
