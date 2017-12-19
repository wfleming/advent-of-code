module Main where

import D11Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile file
  let endAt = moveAll (0, 0) <$> parseResult
  putStrLn $ "p1: walk ends at " ++ (show endAt)
  let p1Dist = dist (0, 0) <$> endAt
  putStrLn $ "p1: distance = " ++ (show p1Dist)
  let p2Dist = furthestDistWalked (0, 0) <$> parseResult
  putStrLn $ "p2: furhtest distance = " ++ (show p2Dist)
