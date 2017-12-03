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
                    Node { pos = (0, 0), info = (86, 73) },
                    Node { pos = (0, 1), info = (88, 65) }
                ]

    describe "viablePairs" $
        it "finds viable pairs" $ do
            let ns = [
                    Node { pos = (0, 0), info = (86, 73) },
                    Node { pos = (0, 1), info = (88, 12) },
                    Node { pos = (0, 2), info = (88, 68) },
                    Node { pos = (0, 3), info = (85, 84) }
                  ]
            viablePairs ns `shouldBe` [
                    (ns !! 1, ns !! 0),
                    (ns !! 0, ns !! 1),
                    (ns !! 2, ns !! 1),
                    (ns !! 1, ns !! 2)
                ]

    describe "path solving" $ do
        it "finds the source node" $
            sourceNode fixtureNodes `shouldBe` (fixtureNodes !! 6)

        it "determines (Node NodeKind) from (Node NodeSizes)" $ do
            let sNodes = simplify fixtureNodes
            (map info sNodes) `shouldBe` [Avail, Avail, Wall, Avail, Empty,
                Avail, Data, Avail, Avail]

        it "moves data from one node to another" $ do
            let moved = move (1, 0) (1, 1) fixtureNodes
            used (get (1, 0) moved) `shouldBe` 0
            used (get (1, 1) moved) `shouldBe` 7

        it "determines viableMoves" $ do
            let moves = viableMoves $ simplify fixtureNodes
            moves `shouldBe` [((0, 1), (1,1)), ((1, 0), (1,1)),
                ((1, 2), (1,1)), ((2, 1), (1,1))]

        it "finds the shortestPath" $ do
            let p = findPath $ simplify fixtureNodes
            length p `shouldBe` 8 -- 7 steps + initial state

fixtureInput = "/dev/grid/node-x0-y0   10T    8T     2T   80%\n" ++
               "/dev/grid/node-x0-y1   11T    6T     5T   54%\n" ++
               "/dev/grid/node-x0-y2   32T   28T     4T   87%\n" ++
               "/dev/grid/node-x1-y0    9T    7T     2T   77%\n" ++
               "/dev/grid/node-x1-y1    8T    0T     8T    0%\n" ++
               "/dev/grid/node-x1-y2   11T    7T     4T   63%\n" ++
               "/dev/grid/node-x2-y0   10T    6T     4T   60%\n" ++
               "/dev/grid/node-x2-y1    9T    8T     1T   88%\n" ++
               "/dev/grid/node-x2-y2    9T    6T     3T   66%\n"

fixtureNodes = either (error . show) id $ P.parse nodesP "fixture" fixtureInput
