module D9Spec where

import Test.Hspec
import Test.QuickCheck

import D9Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "scrubGarbage" $ do
    it "does not change strings with no garbage" $ do
      scrubGarbage "{{}}" `shouldBe` "{{}}"

    it "scrubs simple garbage sequences" $ do
      scrubGarbage "{<22>}" `shouldBe` "{}"

    it "ignores } in garbage" $ do
      scrubGarbage "{<}>}" `shouldBe` "{}"

    it "scrubs escape chars correctly" $ do
      scrubGarbage "{<<<<<<<!!>}" `shouldBe` "{}"
      scrubGarbage "{<<<!!!>>}" `shouldBe` "{}"

  describe "scoreGroups" $ do
    it "scores groups" $ do
      (scoreGroups . scrubGarbage) "{}" `shouldBe` 1
      (scoreGroups . scrubGarbage) "{{{}}}" `shouldBe` 6
      (scoreGroups . scrubGarbage) "{{},{}}" `shouldBe` 5
      (scoreGroups . scrubGarbage) "{{{},{},{{}}}}" `shouldBe` 16
      (scoreGroups . scrubGarbage) "{<a>,<a>,<a>,<a>}" `shouldBe` 1
      (scoreGroups . scrubGarbage) "{{<ab>},{<ab>},{<ab>},{<ab>}}" `shouldBe` 9
      (scoreGroups . scrubGarbage) "{{<!!>},{<!!>},{<!!>},{<!!>}}" `shouldBe` 9
      (scoreGroups . scrubGarbage) "{{<a!>},{<a!>},{<a!>},{<ab>}}" `shouldBe` 3
