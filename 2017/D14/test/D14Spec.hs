module D14Spec where

import Data.List (sort)
import Test.Hspec
import Test.QuickCheck

import D14Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "hexBits" $ do
    it "is the right digits" $ do
      let bs = hexBits "a0c2017"
      concatMap show bs `shouldBe` "1010000011000010000000010111"

  describe "grid" $ do
    it "consructs the expected grid for sample input" $ do
      let g = grid "flqrgnkx"
      rowHead g 0 `shouldBe` "11010100"
      rowHead g 1 `shouldBe` "01010101"
      rowHead g 2 `shouldBe` "00001010"
      rowHead g 3 `shouldBe` "10101101"

  describe "used" $ do
    it "is correct for sample input" $ do
      let g = grid "flqrgnkx"
      used g `shouldBe` 8108

  describe "rowSpans" $ do
    it "finds contiguous spans of 1s" $ do
      let gs = rowSpans sampleGrid 0
      gs `shouldBe` [ [(0, 0), (0, 1)], [(0, 3)], [(0, 5)] ]

    it "finds longer spans of 1s" $ do
      let gs = rowSpans [Row [1, 1, 1, 1, 0, 1]] 0
      gs `shouldBe` [ [(0, 0), (0, 1), (0, 2), (0, 3)], [(0, 5)] ]

  describe "mergeSpans" $ do
    it "merges a span when it is adjacent to previous span" $ do
      let gs = rowSpans sampleGrid 0
      let gs' = mergeSpans gs [(1,1), (1,2)]
      gs' `shouldBe` [ [(0, 0), (0, 1), (1, 1), (1, 2)], [(0, 3)], [(0, 5)] ]

    it "appends a span when it is not adjacent to previous span" $ do
      let gs = rowSpans sampleGrid 0
      let gs' = mergeSpans gs [(1,2)]
      gs' `shouldBe` [ [(0, 0), (0, 1)], [(0, 3)], [(0, 5)], [(1, 2)] ]

  describe "groups" $ do
    it "is the right number of groups for the sample input" $ do
      let g = grid "flqrgnkx"
      length (groups g) `shouldBe` 1242

    it "is the right groups for the mini sample" $ do
      let grps = sort . map sort . groups . take 2 $ sampleGrid
      grps `shouldMatchList` [ [(0, 0), (0, 1), (1, 1)]
                             , [(0, 3), (1, 3)]
                             , [(0, 5), (1, 5)]
                             , [(1, 7)]
                             ]

    it "is right for one big group" $ do
      let grps = groups [ Row [1, 1, 1, 1]
                         , Row [1, 1, 1, 1]
                         , Row [1, 1, 1, 1]
                         , Row [1, 1, 1, 1]
                         ]
      grps `shouldMatchList` [[ (0, 0), (0, 1), (0, 2), (0, 3)
                             , (1, 0), (1, 1), (1, 2), (1, 3)
                             , (2, 0), (2, 1), (2, 2), (2, 3)
                             , (3, 0), (3, 1), (3, 2), (3, 3)
                             ]]

  describe "showGroups" $ do
    it "is a string of the groups" $ do
      let s = showGroups $ take 2 sampleGrid
      s `shouldBe` "11.2.3..\n" ++
                   ".1.2.3.4\n"

rowHead :: [Row] -> Int -> String
rowHead g n = take 8 . show . head $ drop n g

sampleGrid :: Grid
sampleGrid =
  [ Row [1, 1, 0, 1, 0, 1, 0, 0]
  , Row [0, 1, 0, 1, 0, 1, 0, 1]
  , Row [0, 0, 0, 0, 1, 0, 1, 0]
  , Row [1, 0, 1, 0, 1, 1, 0, 1]
  , Row [0, 1, 1, 0, 1, 0, 0, 0]
  , Row [1, 1, 0, 0, 1, 0, 0, 1]
  , Row [0, 1, 0, 0, 0, 1, 0, 0]
  , Row [1, 1, 0, 1, 0, 1, 1, 0]
  ]
