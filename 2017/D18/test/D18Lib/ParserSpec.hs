module D18Lib.ParserSpec where

import Test.Hspec

import D18Lib (Op(..), Operand(..))
import D18Lib.Parser
import Text.Megaparsec (parse)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "op" $ do
    it "parses an op" $ do
      parse op "NA" "set a 1" `shouldBe` Right (Set (RegRef 'a') (Const 1))

  describe "ops" $ do
    it "parses some ops" $ do
      let input = "set a 1\nadd a 2\nmul a a\nmod a 5\nsnd a\nset a 0\nrcv a \njgz a -1\nset a 1\njgz a -2\n"
      parse ops "NA" input `shouldBe` Right
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
