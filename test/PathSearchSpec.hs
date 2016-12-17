module PathSearchSpec (main, spec) where

import Test.Hspec

import PathSearch

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "minPath" $
        it "finds a path" $ do
            minPath (N 1) `shouldBe` Path [N 1, N 3, N 6, N 7, N 14, N 15]

data TestPathState = N Int deriving (Eq, Show)

instance PathState TestPathState where
    nextStates (N i) = if i `mod` 2 == 0
      then [N (i + 1), N (i `div` 2)]
      else [N (i * 2), N (i * 3)]

    isGoal (N i) = i == 15
