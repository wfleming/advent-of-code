module Main where

import D12Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile file
  let g = either (error . show) buildGraph parseResult
  let g0Count = (length . group g) 0
  putStrLn $ "p1: 0 group size = " ++ (show g0Count)
  let groupCount = length . groups $ g
  putStrLn $ "p1: 0 group size = " ++ (show groupCount)
