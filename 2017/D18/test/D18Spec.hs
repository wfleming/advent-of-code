module D18Spec where

import Test.Hspec
import Test.QuickCheck

import D18Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
   describe "runUntil" $ do
     it "runs sample machine correctly" $ do
       let m = newMachine sampleOps
       let m' = runUntil willRecover m
       willRecover m' `shouldBe` True
       mLastPlayed m' `shouldBe` Just 4

   describe "step" $ do
     it "applies the correct state" $ do
       let m0 = newMachine sampleOps
       let m1 = step m0
       mPos m1 `shouldBe` 1
       regVal m1 'a' `shouldBe` 1
       let m2 = step m1
       mPos m2 `shouldBe` 2
       regVal m2 'a' `shouldBe` 3
       let m3 = step m2
       mPos m3 `shouldBe` 3
       regVal m3 'a' `shouldBe` 9

   describe "runSys" $ do
     it "runs the sample system correctly" $ do
       let s = newSystem sampleOpsP2
       let s' = runSys s
       terminated s' `shouldBe` True
       sSndCount0 s' `shouldBe` 3
       sSndCount1 s' `shouldBe` 3
       regVal (sM0 s') 'a' `shouldBe` 1
       regVal (sM1 s') 'a' `shouldBe` 1
       regVal (sM0 s') 'b' `shouldBe` 2
       regVal (sM1 s') 'b' `shouldBe` 2
       regVal (sM0 s') 'c' `shouldBe` 1
       regVal (sM1 s') 'c' `shouldBe` 0


sampleOps =
    [ Set (RegRef 'a') (Const 1)
    , Add (RegRef 'a') (Const 2)
    , Mul (RegRef 'a') (RegRef 'a')
    , Mod (RegRef 'a') (Const 5)
    , Snd (RegRef 'a')
    , Set (RegRef 'a') (Const 0)
    , Rcv (RegRef 'a')
    , Jgz (RegRef 'a') (Const (-1))
    , Set (RegRef 'a') (Const 1)
    , Jgz (RegRef 'a') (Const (-2))
    ]

sampleOpsP2 =
    [ Snd (Const 1)
    , Snd (Const 2)
    , Snd (RegRef 'p')
    , Rcv (RegRef 'a')
    , Rcv (RegRef 'b')
    , Rcv (RegRef 'c')
    , Rcv (RegRef 'd')
    ]
