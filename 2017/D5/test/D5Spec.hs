module D5Spec where

import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec (parse, ParseError, Dec)

import D5Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
    it "parses list of ints" $ do
      let r = parse parser "NO_INPUT_FILE" "1\n-1\n0\n"
      r `shouldBe` Right [1, -1, 0]

  describe "stepP1" $ do
    it "makes correct moves" $ do
      let s0 = State { pos = 0, jmps = [0, 3, 0, 1, -3] }
      let s1 = stepP1 s0
      s1 `shouldBe` Just State { pos = 0, jmps = [1, 3, 0, 1, -3] }
      let s2 = s1 >>= stepP1
      s2 `shouldBe` Just State { pos = 1, jmps = [2, 3, 0, 1, -3] }

  describe "run" $ do
    it "makes the correct number of moves for part 1" $ do
      let s0 = State { pos = 0, jmps = [0, 3, 0, 1, -3] }
      let r = run stepP1 s0
      -- 5 steps were taken, so 6 states total
      length r `shouldBe` 6

    it "makes the correct number of moves for part 2" $ do
      let s0 = State { pos = 0, jmps = [0, 3, 0, 1, -3] }
      let r = run stepP2 s0
      -- 10 steps were taken, so 11 states total
      length r `shouldBe` 11

  describe "run'" $ do
    it "makes the correct number of moves for part 1" $ do
      let s0 = State { pos = 0, jmps = [0, 3, 0, 1, -3] }
      let r = run' stepP1 s0
      -- 5 steps were taken, so 6 states total
      r `shouldBe` 6

    it "makes the correct number of moves for part 2" $ do
      let s0 = State { pos = 0, jmps = [0, 3, 0, 1, -3] }
      let r = run' stepP2 s0
      -- 10 steps were taken, so 11 states total
      r `shouldBe` 11
