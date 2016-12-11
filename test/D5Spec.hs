module D5Spec (main, spec) where

import Test.Hspec

import D5Lib
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    -- These MD5 specs are slow: turned off except when I need to test them

    {-describe "password" $-}
        {-it "finds the password" $ do-}
            {-password "abc" `shouldBe` "18f47a30"-}

    {-describe "orderedPassword" $ do-}
        {-it "finds the password" $ do-}
            {-orderedPassword "abc" `shouldBe` "05ace8e3"-}

    describe "passChar" $ do
        it "is a Just char for a matching index" $ do
            passChar "abc" 3231929 `shouldBe` Just '1'
            passChar "abc" 5017308 `shouldBe` Just '8'

        it "is a Nothing for a non-matching index" $ do
            passChar "abc" 3231928 `shouldBe` Nothing

    describe "orderedPassChar" $ do
        it "is a char & idx for a matching index" $ do
            orderedPassChar "abc" 3231929 `shouldBe` Just ('5', 1)
            orderedPassChar "abc" 5357525 `shouldBe` Just ('e', 4)

        it "is Nothing when idx is invalid" $ do
            orderedPassChar "abc" 5017308 `shouldBe` Nothing
