module D10Spec where

import Test.Hspec
import Test.QuickCheck

import Text.Megaparsec (parse, ParseError, Dec)
import D10Lib

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "parser" $ do
    it "parses a list of ints" $ do
      let r = parse parser "NO_INPUT_FILE" "123,456,78,9\n"
      r `shouldBe` Right [123, 456, 78, 9]
