module D18Spec (main, spec) where

import Test.Hspec

import D18Lib
import qualified Data.Vector as V
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "parsing" $ do
        it "parses a row" $ do
            let r = P.parse rowP "fixture" ".^\n"
            r `shouldBe` Right (V.fromList [Safe, Trap])

    describe "succRow" $ do
        it "builds the next row" $ do
            let r0 = V.fromList [Safe, Safe, Trap, Trap, Safe]
            let r1 = V.fromList [Safe, Trap, Trap, Trap, Trap]
            let r2 = V.fromList [Trap, Trap, Safe, Safe, Trap]
            succRow r0 `shouldBe` r1
            succRow r1 `shouldBe` r2

    describe "nsafe" $ do
        it "counts safe tiles" $ do
            let r0 = V.fromList [Safe, Safe, Trap, Trap, Safe]
            nsafe r0 3 `shouldBe` 6
