module D13Spec where

import Test.Hspec
import Test.QuickCheck

import D13Lib
import Text.Megaparsec (parse)

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "layerParser" $ do
    it "parses a line" $ do
      parse layerParser "NA" "1: 3" `shouldBe` Right (Layer 1 3 0 1)

  describe "layersParser" $ do
    it "parses lines" $ do
      let s = "0: 3\n4: 2\n"
      let expected = [Layer 0 3 0 1, Layer 4 2 0 1]
      parse layersParser "NA" s `shouldBe` Right expected

  describe "stepLayer" $ do
    it "walks back and forth" $ do
      let l = Layer 0 3 0 1
      let l1 = stepLayer l
      l1 `shouldBe` Layer 0 3 1 1
      let l2 = stepLayer l1
      l2 `shouldBe` Layer 0 3 2 1
      let l3 = stepLayer l2
      l3 `shouldBe` Layer 0 3 1 (-1)
      let l4 = stepLayer l3
      l4 `shouldBe` Layer 0 3 0 (-1)
      let l5 = stepLayer l4
      l5 `shouldBe` Layer 0 3 1 1

  describe "runFirewall" $ do
    it "runs to the end with correct severity" $ do
      let ls = [Layer 0 3 0 1, Layer 1 2 0 1, Layer 4 4 0 1, Layer 6 4 0 1]
      let f = initFirewall ls
      let (Firewall _ _ s) = runFirewall f
      s `shouldBe` 24

  describe "findDelay" $ do
    it "runs to the end with correct severity" $ do
      let ls = [Layer 0 3 0 1, Layer 1 2 0 1, Layer 4 4 0 1, Layer 6 4 0 1]
      let f = initFirewall ls
      let d = findDelay f
      d `shouldBe` 10

    it "sanity check: the given sample solution passes" $ do
      let ls = [Layer 0 3 0 1, Layer 1 2 0 1, Layer 4 4 0 1, Layer 6 4 0 1]
      let f = Firewall ls (-11) 0
      let (Firewall _ _ s) = runFirewall f
      s `shouldBe` 0

  describe "findDelay'" $ do
    it "runs to the end with correct severity" $ do
      let ls = [Layer 0 3 0 1, Layer 1 2 0 1, Layer 4 4 0 1, Layer 6 4 0 1]
      let f = initFirewall ls
      let d = findDelay' f
      d `shouldBe` 10

  describe "layerAtT" $ do
    it "knows where a layer is at time t" $ do
      layerAtT (Layer 0 3 0 0) 0 `shouldBe` Layer 0 3 0 0
      layerAtT (Layer 0 3 0 0) 1 `shouldBe` Layer 0 3 1 0
      layerAtT (Layer 0 3 0 0) 2 `shouldBe` Layer 0 3 2 0
      layerAtT (Layer 0 3 0 0) 3 `shouldBe` Layer 0 3 1 0
      layerAtT (Layer 0 3 0 0) 4 `shouldBe` Layer 0 3 0 0
      layerAtT (Layer 0 3 0 0) 5 `shouldBe` Layer 0 3 1 0

      layerAtT (Layer 0 2 0 0) 0 `shouldBe` Layer 0 2 0 0
      layerAtT (Layer 0 2 0 0) 1 `shouldBe` Layer 0 2 1 0
      layerAtT (Layer 0 2 0 0) 2 `shouldBe` Layer 0 2 0 0

      layerAtT (Layer 0 4 0 0) 0 `shouldBe` Layer 0 4 0 0
      layerAtT (Layer 0 4 0 0) 1 `shouldBe` Layer 0 4 1 0
      layerAtT (Layer 0 4 0 0) 2 `shouldBe` Layer 0 4 2 0
      layerAtT (Layer 0 4 0 0) 3 `shouldBe` Layer 0 4 3 0
      layerAtT (Layer 0 4 0 0) 4 `shouldBe` Layer 0 4 2 0
      layerAtT (Layer 0 4 0 0) 5 `shouldBe` Layer 0 4 1 0
      layerAtT (Layer 0 4 0 0) 6 `shouldBe` Layer 0 4 0 0
      layerAtT (Layer 0 4 0 0) 7 `shouldBe` Layer 0 4 1 0
