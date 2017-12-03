module D3Spec where

import Test.Hspec
import Test.QuickCheck

import D3Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "dist" $ do
    it "gets the manhattan distance" $ do
      dist (1, 0) (0, 0) `shouldBe` 1
      dist (1, 1) (0, 0) `shouldBe` 2

  describe "cellPos" $ do
    it "gets the coords of a cell by id" $ do
      cellPos 1 `shouldBe` (0, 0)
      cellPos 2 `shouldBe` (1, 0)
      cellPos 3 `shouldBe` (1, 1)
      cellPos 4 `shouldBe` (0, 1)
      cellPos 5 `shouldBe` (-1, 1)

  describe "p1 integration" $ do
    it "gets the correct distance for cell n=1" $ do
      let d = dist (0, 0) $ cellPos 1
      d `shouldBe` 0

    it "gets the correct distance for cell n=12" $ do
      let d = dist (0, 0) $ cellPos 12
      d `shouldBe` 3

    it "gets the correct distance for cell n=23" $ do
      let d = dist (0, 0) $ cellPos 23
      d `shouldBe` 2

    it "gets the correct distance for cell n=1024" $ do
      let d = dist (0, 0) $ cellPos 1024
      d `shouldBe` 31

  describe "neighbors" $ do
    it "knows when coords are neighbors are not" $ do
      neighbors (0, 0) (1, 0) `shouldBe` True
      neighbors (0, 0) (1, 1) `shouldBe` True
      neighbors (0, 0) (2, 0) `shouldBe` False

  describe "cellNeighbors" $ do
    it "identifies the neighbors of a cell" $ do
      cellNeighbors 2 `shouldBe` [1]
      cellNeighbors 3 `shouldBe` [1, 2]

  describe "stressTestVal" $ do
    it "calcs the value at cell 1" $ do
      stressTestVal 1 `shouldBe` 1

    it "calcs the value at cell 2" $ do
      stressTestVal 2 `shouldBe` 1

    it "calcs the value at cell 3" $ do
      stressTestVal 3 `shouldBe` 2

    it "calcs the value at cell 4" $ do
      stressTestVal 4 `shouldBe` 4

    it "calcs the value at cell 5" $ do
      stressTestVal 5 `shouldBe` 5

    it "calcs the value at cell 7" $ do
      stressTestVal 7 `shouldBe` 11

  describe "firstValHigherThan" $ do
    it "finds the first higher val" $ do
      firstValHigherThan 2 `shouldBe` 4
      firstValHigherThan 7 `shouldBe` 6
