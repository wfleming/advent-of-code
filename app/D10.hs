module Main (main) where

import D10Lib
import Data.List
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile rulesP file
  let rules = either (error . show) id parseResult
  let graph = graphFromAST rules

  let bot_17_61 = find (\b -> (Just 17) == (c1 b) && (Just 61) == (c2 b)) graph
  putStrLn $ "bot that handles 17 & 61: " ++ show bot_17_61

  let o0c = outputCard rules graph 0
  let o1c = outputCard rules graph 1
  let o2c = outputCard rules graph 2
  putStrLn $ "product of cards in outputs 0-2: " ++ show (o0c * o1c * o2c)

