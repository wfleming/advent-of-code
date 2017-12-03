module D10Spec (main, spec) where

import Test.Hspec

import D10Lib
import Data.List
import qualified Text.Parsec as P

main :: IO ()
main = hspec spec

spec :: Spec
spec = parallel $ do
    describe "rulesP" $
        it "parses" $ do
            let p = P.parse rulesP "fixture"
            p "value 5 goes to bot 2\nbot 2 gives low to bot 1 and high to output 0\n"
                `shouldBe`
                Right [ASTValue 5 2, ASTBot 2 (DestBot 1) (DestOutput 0)]

    describe "emptyBots" $
        it "builds some bots" $
            sort (emptyBots fixtureRules) `shouldBe`
                [ Bot { num = 0, c1 = Nothing, c2 = Nothing }
                , Bot { num = 1, c1 = Nothing, c2 = Nothing }
                , Bot { num = 2, c1 = Nothing, c2 = Nothing }
                ]

    describe "startBots" $
        it "builds some bots" $
            sort (startBots fixtureRules) `shouldBe`
                [ Bot { num = 0, c1 = Nothing, c2 = Nothing }
                , Bot { num = 1, c1 = Just 3, c2 = Nothing }
                , Bot { num = 2, c1 = Just 2, c2 = Just 5 }
                ]

    describe "graph" $
        it "builds some bots" $
            sort (graphFromAST fixtureRules) `shouldBe`
                [ Bot { num = 0, c1 = Just 3, c2 = Just 5 }
                , Bot { num = 1, c1 = Just 2, c2 = Just 3 }
                , Bot { num = 2, c1 = Just 2, c2 = Just 5 }
                ]

    describe "replace and addCard" $ do
        it "replaces the bot with the updated record" $ do
            let bs = [Bot { num = 0, c1 = Just 5, c2 = Nothing }, Bot { num = 1, c1 = Just 3, c2 = Nothing }, Bot { num = 2, c1 = Just 5, c2 = Nothing }]
            replace bs (addCard (bs !! 1) 7) `shouldBe`
                [ Bot { num = 0, c1 = Just 5, c2 = Nothing }
                , Bot { num = 1, c1 = Just 3, c2 = Just 7 }
                , Bot { num = 2, c1 = Just 5, c2 = Nothing }
                ]

        it "keeps c1, c2 of bot sorted" $ do
            let b = Bot { num = 0, c1 = Just 2, c2 = Nothing }
            addCard b 1 `shouldBe` Bot { num = 0, c1 = Just 1, c2 = Just 2 }

    describe "outputCard" $
        it "finds the card" $ do
            let graph = graphFromAST fixtureRules
            outputCard fixtureRules graph 1 `shouldBe` 2
            outputCard fixtureRules graph 0 `shouldBe` 5
            outputCard fixtureRules graph 2 `shouldBe` 3


fixtureRules =
    [ ASTValue 5 2
    , ASTBot 2 (DestBot 1) (DestBot 0)
    , ASTValue 3 1
    , ASTBot 1 (DestOutput 1) (DestBot 0)
    , ASTBot 0 (DestOutput 2) (DestOutput 0)
    , ASTValue 2 2
    ]

