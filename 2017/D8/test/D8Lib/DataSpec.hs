module D8Lib.DataSpec where

import Test.Hspec

import qualified Data.Map.Strict as M
import D8Lib.Data

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "initState" $ do
    it "constructs an appropriate map" $ do
      let s = initState sampleRules
      s `shouldBe` M.fromList [("a", 0), ("b", 0), ("c", 0)]

  describe "testCnd" $ do
    it "test a condition against the map" $ do
      let s = initState sampleRules
      let c = Cmp "a" Gt 1
      testCnd c s `shouldBe` False

  describe "apply" $ do
    it "test a condition against the map" $ do
      let s = initState sampleRules
      let s' = M.insert "a" 3 s
      let s'' = apply s' (head sampleRules)
      M.lookup "a" s'' `shouldBe` Just 3
      M.lookup "b" s'' `shouldBe` Just 5

  describe "sample full run" $ do
    it "sets appropraite states" $ do
      let s0 = initState sampleRules
      s0 `shouldBe` M.fromList [("a", 0), ("b", 0), ("c", 0)]

      let s1 = apply s0 (sampleRules !! 0)
      s1 `shouldBe` M.fromList [("a", 0), ("b", 0), ("c", 0)]

      let r1@(Rule r1n r1a r1c) = sampleRules !! 1
      testCnd r1c s1 `shouldBe` True
      let s2 = apply s1 r1
      s2 `shouldBe` M.fromList [("a", 1), ("b", 0), ("c", 0)]

      let s3 = apply s2 (sampleRules !! 2)
      s3 `shouldBe` M.fromList [("a", 1), ("b", 0), ("c", 10)]

      let s4 = apply s3 (sampleRules !! 3)
      s4 `shouldBe` M.fromList [("a", 1), ("b", 0), ("c", (-10))]

sampleRules =
  [ Rule "b" (Inc 5) (Cmp "a" Gt 1)
  , Rule "a" (Inc 1) (Cmp "b" Lt 5)
  , Rule "c" (Dec (-10)) (Cmp "a" Gte 1)
  , Rule "c" (Inc (-20)) (Cmp "c" Eq 10) ]
