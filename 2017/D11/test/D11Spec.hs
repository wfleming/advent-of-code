module D11Spec where

import Test.Hspec
import Test.QuickCheck

import D11Lib
import Text.Megaparsec (parse, ParseError, Dec)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "move" $ do
    it "moves to the right place" $ do
      move (0, 0) N `shouldBe` (0, -1)
      move (0, 0) NE `shouldBe` (1, -1)
      move (1, 1) NE `shouldBe` (2, 0)

  describe "cube" $ do
    it "calculates z" $ do
      cube (0, 0) `shouldBe` (0, 0, 0)
      cube (1, 1) `shouldBe` (1, -2, 1)
      cube (1, 0) `shouldBe` (1, -1, 0)
      cube (2, 1) `shouldBe` (2, -3, 1)

  describe "dist" $ do
    it "is the correct distance" $ do
      dist (0, 4) (3, 2) `shouldBe` 3
      dist (0, 0) (2, 1) `shouldBe` 3

  describe "parser" $ do
    it "parses a list of moves" $ do
      parse parser "NA" "ne\n" `shouldBe` Right [NE]
      let r = parse parser "NA" "ne,nw,n,s\n"
      r `shouldBe` Right [NE, NW, N, S]

  describe "parseDir" $ do
    it "parses a string" $ do
      parse parseDir "NA" "nw" `shouldBe` Right NW
      parse parseDir "NA" "n" `shouldBe` Right N
      parse parseDir "NA" "ne" `shouldBe` Right NE
      parse parseDir "NA" "se" `shouldBe` Right SE
      parse parseDir "NA" "s" `shouldBe` Right S
      parse parseDir "NA" "sw" `shouldBe` Right SW

  describe "integration" $ do
    it "gets to the right place via different paths" $ do
      move (0, 0) NE `shouldBe` (1, -1)
      move (1, -1) NE `shouldBe` (2, -2)
      move (2, -2) S `shouldBe` (2, -1)
      move (2, -1) S `shouldBe` (2, 0)

      move (0, 0) SE `shouldBe` (1, 0)
      move (1, 0) SE `shouldBe` (2, 0)

    it "moves & knows the distance" $ do
      let p0 = moveAll (0, 0) [NE, NE, NE]
      dist (0, 0) p0 `shouldBe` 3

      let p1 = moveAll (0, 0) [NE, NE, SW, SW]
      dist (0, 0) p1 `shouldBe` 0

      let p2 = moveAll (0, 0) [NE, NE, S, S]
      p2 `shouldBe` (moveAll (0, 0) [SE, SE])
      dist (0, 0) p2 `shouldBe` 2

      let p3 = moveAll (0, 0) [SE, SW, SE, SW, SW]
      dist (0, 0) p3 `shouldBe` 3
