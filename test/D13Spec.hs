module D13Spec (main, spec) where

import Test.Hspec

import D13Lib
import qualified PathSearch as PS

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "num2bin" $
        it "formats numbers" $ do
            num2bin 1 `shouldBe` "1"
            num2bin 2 `shouldBe` "10"
            num2bin 10 `shouldBe` "1010"

    describe "pointIsOpen" $
        it "is correct" $ do
          open MazeState { point = (1, 1), key = 10, goal = (0, 0) } `shouldBe` True
          open MazeState { point = (1, 0), key = 10, goal = (0, 0) } `shouldBe` False
          open MazeState { point = (5, 1), key = 10, goal = (0, 0) } `shouldBe` False
          open MazeState { point = (2, 6), key = 10, goal = (0, 0) } `shouldBe` True

    describe "pathfinding" $
        it "finds the shortest path through the maze" $ do
          let start = MazeState { point = (1, 1), key = 10, goal = (7, 4) }
          let path = PS.minPath start
          PS.length path `shouldBe` 12 -- 11 steps
