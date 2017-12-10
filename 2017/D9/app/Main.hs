module Main where

import D9Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  input <- readFile file
  let scrubbed = scrubGarbage input
  let groupScore = scoreGroups scrubbed
  putStrLn $ "p1: score = " ++ (show groupScore)
  let garbageCount = countGarbage input
  putStrLn $ "p2: garbage char count = " ++ (show garbageCount)
