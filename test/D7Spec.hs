module D7Spec (main, spec) where

import Test.Hspec

import D7Lib
import Data.List

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "supportsTLS" $
        it "works" $ do
            supportsTLS emptyTLSContext "abba[mnop]qrst" `shouldBe` True
            supportsTLS emptyTLSContext "abcd[bddb]xyyx" `shouldBe` False
            supportsTLS emptyTLSContext "aaaa[qwer]tyui" `shouldBe` False
            supportsTLS emptyTLSContext "ioxxoj[asdfgh]zxcvbn" `shouldBe` True
            supportsTLS emptyTLSContext "abba[xyyx]qrst" `shouldBe` False

    describe "supportsSSL" $
        it "works" $ do
            supportsSSL emptySSLContext "aba[bab]xyz" `shouldBe` True
            supportsSSL emptySSLContext "xyx[xyx]xyx" `shouldBe` False
            supportsSSL emptySSLContext "aaa[kek]eke" `shouldBe` True
            supportsSSL emptySSLContext "zazbz[bzb]cdb" `shouldBe` True
