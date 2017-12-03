module D11LibSpec (main, spec) where

import Test.Hspec

import D11Lib
import Data.List
import qualified PathSearch as PS

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "State" $ do
        describe "Eq" $ do
            it "is equal even if floor elements are in a different order" $ do
                let s1 = State {
                    floors = [Floor [(Microchip, Curium), (Generator, Curium)], Floor []]
                    , curFloor = 1
                    }
                let s2 = State {
                    floors = [Floor [(Generator, Curium), (Microchip, Curium)], Floor []]
                    , curFloor = 1
                    }
                s1 == s2 `shouldBe` True

        describe "isGoal" $ do
            it "is False when things are on several floors" $ do
                let s = State {
                    floors = [Floor [(Microchip, Curium)], Floor []]
                    , curFloor = 1
                    }
                PS.isGoal s `shouldBe` False

            it "is False when not currently at top floor" $ do
                let s = State {
                    floors = [Floor [], Floor [(Generator, Curium)]]
                    , curFloor = 0
                    }
                PS.isGoal s `shouldBe` False

            it "is True when currently at top floor, and all items on that floor" $ do
                let s = State {
                    floors = [Floor [], Floor [(Microchip, Curium)]]
                    , curFloor = 1
                    }
                PS.isGoal s `shouldBe` True

        describe "goalDist" $ do
            it "calculates distance to goal state" $ do
                let s = State {
                    floors = [Floor [(Microchip, Curium)], Floor []]
                    , curFloor = 0
                    }
                PS.goalDist s `shouldBe` 2

                let s' = State {
                    floors = [Floor [], Floor [(Microchip, Curium)]]
                    , curFloor = 1
                    }
                PS.goalDist s' `shouldBe` 0

        describe "nextStates" $ do
            it "does stuff" $ do
                let s = State {
                    floors = [
                      Floor [],
                      Floor [(Microchip, Plutonium), (Generator, Plutonium)],
                      Floor [(Microchip, Curium)]
                      ]
                    , curFloor = 1
                    }
                PS.nextStates s `shouldBe`
                    [ State {
                        floors = [
                          Floor [(Microchip, Plutonium)],
                          Floor [(Generator, Plutonium)],
                          Floor [(Microchip, Curium)]
                          ]
                        , curFloor = 0
                      },
                      State {
                        floors = [
                          Floor [(Generator, Plutonium)],
                          Floor [(Microchip, Plutonium)],
                          Floor [(Microchip, Curium)]
                          ]
                        , curFloor = 0
                      },
                      State {
                        floors = [
                          Floor [(Microchip, Plutonium), (Generator, Plutonium)],
                          Floor [],
                          Floor [(Microchip, Curium)]
                          ]
                        , curFloor = 0
                      },
                      State {
                        floors = [
                          Floor [],
                          Floor [(Generator, Plutonium)],
                          Floor [(Microchip, Curium), (Microchip, Plutonium)]
                          ]
                        , curFloor = 2
                      }
                    ]
            it "finds a goal state next when available" $ do
                let s = State {
                    floors = [
                        Floor {items = []},
                        Floor {items = []},
                        Floor {items = [(Microchip,Promethium), (Microchip,Cobalt)]},
                        Floor {items = [(Generator,Cobalt), (Generator,Promethium)]}
                    ],
                    curFloor = 2
                  }
                let goal = State {
                    floors = [
                        Floor {items = []},
                        Floor {items = []},
                        Floor {items = []},
                        Floor {items = [(Microchip,Promethium), (Microchip,Cobalt), (Generator,Cobalt), (Generator,Promethium)]}
                    ],
                    curFloor = 3
                  }
                let states = PS.nextStates s
                goal `shouldSatisfy` PS.isGoal
                states `shouldSatisfy` (goal `elem`)


        describe "valid" $ do
            it "is False when chip is unprotected & there's a generator" $ do
                let s = State {
                    floors = [
                        Floor []
                        , Floor [(Generator, Curium), (Microchip, Plutonium)]
                        ]
                    , curFloor = 1
                    }
                s `shouldSatisfy` (not . valid)

            it "is False when 1 chip is protected but other isn't" $ do
                let s = State {
                    floors = [
                        Floor [(Generator, Curium), (Microchip, Curium), (Microchip, Plutonium)]
                        , Floor []
                        ]
                    , curFloor = 1
                    }
                s `shouldSatisfy` (not . valid)

            it "is True when chip is protected" $ do
                let s = State {
                    floors = [
                        Floor [(Generator, Curium), (Generator, Plutonium), (Microchip, Plutonium)]
                        ]
                    , curFloor = 0
                    }
                s `shouldSatisfy` valid

            it "is True when chip is unprotected but there's no generator" $ do
                let s = State {
                    floors = [
                      Floor [(Microchip, Plutonium)]
                      ]
                    , curFloor = 0
                    }
                s `shouldSatisfy` valid

        describe "applyMove" $
            it "changes the state" $ do
                let s = State {
                    floors = [
                      Floor [],
                      Floor [(Microchip, Plutonium), (Generator, Plutonium)],
                      Floor [(Microchip, Curium)]
                      ]
                    , curFloor = 1
                    }
                applyMove s 1 [(Microchip, Plutonium)] `shouldBe` State {
                    floors = [
                      Floor [],
                      Floor [(Generator, Plutonium)],
                      Floor [(Microchip, Curium), (Microchip, Plutonium)]
                      ]
                    , curFloor = 2
                    }
