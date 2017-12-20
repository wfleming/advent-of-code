module D12Spec where

import Test.Hspec
import Test.QuickCheck

import D12Lib
import Data.List (sort)
import Text.Megaparsec (parse)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parseLine" $ do
    it "parses lines" $ do
      parse parseLine "NA" "1 <-> 2" `shouldBe` Right (1, [2])
      parse parseLine "NA" "1 <-> 2, 3, 4" `shouldBe` Right (1, [2, 3, 4])

  describe "parser" $ do
    it "parses" $ do
      let s = "1 <-> 2\n2 <-> 3, 4\n"
      let expected = [(1, [2]), (2, [3, 4])]
      parse parser "NA" s `shouldBe` Right expected

  describe "buildBounds" $ do
    it "gets the min/max node indexes" $ do
      let d = [(1, [2]), (2, [1, 3]), (3, [])]
      buildBounds d `shouldBe` (1, 3)

  describe "buildEdges" $ do
    it "constructs edges" $ do
      let d = [(1, [2]), (2, [1, 3]), (3, [])]
      buildEdges d `shouldBe` [(1, 2), (2, 1), (2, 3)]

  describe "group" $ do
    it "knows group memberships" $ do
      let g = buildGraph sampleData
      group g 0 `shouldBe` [0, 2, 3, 4, 5, 6]
      group g 1 `shouldBe` [1]

  describe "groups" $ do
    it "returns all groups" $ do
      let g = buildGraph sampleData
      groups g `shouldBe` [ [0, 2, 3, 4, 5, 6], [1] ]

sampleData = [ (0, [2])
             , (1, [1])
             , (2, [0, 3, 4])
             , (3, [2, 4])
             , (4, [2, 3, 6])
             , (5, [6])
             , (6, [4, 5])
             ]
