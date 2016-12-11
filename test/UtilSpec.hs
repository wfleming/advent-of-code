module UtilSpec (main, spec) where

import Test.Hspec

import qualified Util as U

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "freq" $
        it "gets frequencies of chars in a string" $
            U.freq "ab-az-bb" `shouldBe` [('a', 2), ('b', 3), ('z', 1)]
