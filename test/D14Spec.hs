module D14Spec (main, spec) where

import Test.Hspec

import D14Lib

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "rep3Char" $
        it "find repeated chars, or nothing" $ do
            rep3Char "abc" `shouldBe` Nothing
            rep3Char "aaa" `shouldBe` Just 'a'
            rep3Char "caaa" `shouldBe` Just 'a'
            rep3Char "b344444zzzz" `shouldBe` Just '4'
            rep3Char "abcczc" `shouldBe` Nothing

    describe "rep5Char" $
        it "indicate if char is in string 5 times consecutively" $ do
            rep5Char 'a' "abc" `shouldBe` False
            rep5Char '4' "b344444zzzz" `shouldBe` True
            rep5Char '5' "b344444zzzz" `shouldBe` False

    describe "isKey" $ do
        it "is True when conditions match" $ do
          let hs = buildHashes [39..] "abc"
          isKey hs `shouldBe` True

        it "is false when conditions do not match" $ do
          let hs = buildHashes [18..] "abc"
          isKey hs `shouldBe` False

    describe "buildHashes2" $
        it "runs the correct number of iterations" $ do
            let hs = buildHashes2 [0] "abc"
            (head hs) `shouldBe` (0, "a107ff634856bb300138cac6568c0f24")

    describe "findKeys" $ do
        it "gets the right keys" $ do
          let ks = findKeys "abc"
          length ks `shouldBe` keyCount
          (fst . last) ks `shouldBe` 22728
