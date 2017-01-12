module D22Spec (main, spec) where

import Test.Hspec

import D22Lib
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "parsing" $
        it "parses instructions" $ do
            let input =
                  "/dev/grid/node-x0-y0     86T   73T    13T   84%\n" ++
                  "/dev/grid/node-x0-y1     88T   65T    23T   73%\n"
            let ns = P.parse nodesP "fixture" input
            ns `shouldBe` Right [
                    Node { pos = (0, 0), capacity = 86, used = 73 },
                    Node { pos = (0, 1), capacity = 88, used = 65 }
                ]

    describe "viablePairs" $
        it "finds viable pairs" $ do
            let ns = [
                    Node { pos = (0, 0), capacity = 86, used = 73 },
                    Node { pos = (0, 1), capacity = 88, used = 12 },
                    Node { pos = (0, 2), capacity = 88, used = 68 },
                    Node { pos = (0, 3), capacity = 85, used = 84 }
                  ]
            viablePairs ns `shouldBe` [
                    (ns !! 1, ns !! 0),
                    (ns !! 0, ns !! 1),
                    (ns !! 2, ns !! 1),
                    (ns !! 1, ns !! 2)
                ]
