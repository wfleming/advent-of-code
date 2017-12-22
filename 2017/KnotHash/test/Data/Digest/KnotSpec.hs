module Data.Digest.KnotSpec where

import Test.Hspec
import Data.Digest.Knot

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "step" $ do
    it "steps a small example correctly" $ do
      let s0 = new [0..4] [3, 4, 1, 5]
      s0 `shouldBe` State [0, 1, 2, 3, 4] [3, 4, 1, 5] 0 0

      let s1 = step s0
      s1 `shouldBe` State [2, 1, 0, 3, 4] [4, 1, 5] 3 1

      let s2 = step s1
      s2 `shouldBe` State [4, 3, 0, 1, 2] [1, 5] 3 2

      let s3 = step s2
      s3 `shouldBe` State [4, 3, 0, 1, 2] [5] 1 3

      let s4 = step s3
      s4 `shouldBe` State [3, 4, 2, 1, 0] [] 4 4

  describe "seedBytes" $ do
    it "converts chars to bytes & adds standard suffix" $ do
      let bs = seedBytes "1,2,3"
      bs `shouldBe` [49,44,50,44,51,17,31,73,47,23]

    it "gets standard suffix from empty string" $ do
      let bs = seedBytes ""
      bs `shouldBe` [17,31,73,47,23]

  describe "xorList" $ do
    it "xors the list correctly" $ do
      xorList [65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22] `shouldBe` 64

  describe "compactBytes" $ do
    it "chunks & xors the list" $ do
      let xs =
            [ 65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22
            , 65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22
            , 65, 27, 9, 1, 4, 3, 40, 50, 91, 7, 6, 0, 2, 5, 68, 22
            ]
      compactBytes xs `shouldBe` [64, 64, 64]

  describe "hash" $ do
    it "hashes some known strings" $ do
      hash "" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
      hash "AoC 2017" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
      hash "1,2,3" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
      hash "1,2,4" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"

    it "ignores whitespace" $ do
      hash "\n" `shouldBe` "a2582a3a0e66e6e86e3812dcb672a272"
      hash " AoC 2017\n" `shouldBe` "33efeb34ea91902bb2f59c9920caa6cd"
      hash "1,2,3\n" `shouldBe` "3efbe78a8d82f29979031a4aa0b16a9d"
      hash "1,2,4\n" `shouldBe` "63960835bcdc130f0b66d7ff4f6a5a8e"
