module D2Spec where

import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec

import D2Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "rowParser" $ do
    it "parses a row" $ do
      let r = parse rowParser "NO_INPUT_FILE" "1 2   42\t40  5  \n"
      r `shouldBe` Right [1, 2, 42, 40, 5]

  describe "sheetParser" $ do
    it "parses a sheet" $ do
      let r = parse sheetParser "NO_INPUT_FILE" "1 2 \n3   42\n5 6\n"
      r `shouldBe` Right [[1, 2], [3, 42], [5, 6]]

  describe "checksum1" $ do
    it "calcs the checksum of a sheet" $ do
      let s = [ [5, 1, 9, 5],
                [7, 5, 3],
                [2, 4, 6, 8] ]
      checksum1 s `shouldBe` 18

  describe "rowsum1" $ do
    it "calcs the checksum of a row" $ do
      let r = rowsum1 [3, 1, 2, 9, 4]
      r `shouldBe` 8

  describe "checksum2" $ do
    it "calcs the checksum of a sheet" $ do
      let s = [ [5, 9, 2, 8],
                [9, 4, 7, 3],
                [3, 8, 6, 5] ]
      checksum2 s `shouldBe` 9

  describe "rowsum2" $ do
    it "calcs the checksum of a row" $ do
      let r = rowsum2 [5, 9, 2, 8]
      r `shouldBe` 4

  describe "rowfactor" $ do
    it "finds the elements that factor each other" $ do
      rowfactor [5, 9, 2, 8] `shouldBe` (8, 2)
