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

  describe "integration" $ do
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

