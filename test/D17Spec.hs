module D17Spec (main, spec) where

import Test.Hspec

import D17Lib
import qualified PathSearch as PS

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "next moves" $ do
        it "gets correct next moves 1" $ do
            let v = Vault
                  { pos = (0, 0)
                  , height = 4
                  , width = 4
                  , seed = "hijkl"
                  , path = ""
                  }
            validMoves v `shouldBe` [D]
            validMoves (move v D) `shouldBe` [U, R]

    describe "move" $ do
        it "moves" $ do
            let v = Vault
                  { pos = (0, 0)
                  , height = 4
                  , width = 4
                  , seed = "hijkl"
                  , path = ""
                  }
            move v D `shouldBe` Vault
                  { pos = (0, 1)
                  , height = 4
                  , width = 4
                  , seed = "hijkl"
                  , path = "D"
                  }

    describe "path finding" $
        it "finds a path" $ do
            let s = start "ihgpwlah"
            (path . last . PS.states . PS.minPath) s `shouldBe` "DDRRRD"

    describe "maxPath" $
        it "finds a very long path" $ do
            let s = start "ihgpwlah"
            (length . path . maxPath) s `shouldBe` 370
