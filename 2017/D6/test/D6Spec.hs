module D6Spec where

import Test.Hspec
import Text.Megaparsec (parse, ParseError, Dec)

import D6Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
    it "parses list of ints" $ do
      let r = parse parser "NO_INPUT_FILE" "1\t2\t0\n"
      r `shouldBe` Right [1, 2, 0]

  describe "reallocate" $ do
    it "distributes the memory correctly" $ do
      let s0 = [0, 2, 7, 0]
      reallocate s0 `shouldBe` [2, 4, 1, 2]

  describe "reallocateUntilDupe" $ do
    it "iterates until it sees a duplicate state" $ do
      let s0 = [0, 2, 7, 0]
      let ss = reallocateUntilDupe [s0]
      length ss `shouldBe` 6

  describe "cycleLength" $ do
    it "counts the cycle length" $ do
      let s0 = [0, 2, 7, 0]
      let ss = reallocateUntilDupe [s0]
      cycleLength ss `shouldBe` Just 4
