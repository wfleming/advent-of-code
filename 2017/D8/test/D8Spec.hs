module D8Spec where

import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec (parse, ParseError, Dec)
import D8Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "todo" $ do
    it "todo" $ do
      True `shouldBe` True
