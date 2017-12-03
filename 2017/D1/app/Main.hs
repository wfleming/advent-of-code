module Main where

import D1Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  inputStr <- head <$> getArgs
  let parseResult = parseStr inputStr
  let nums = either (error . show) id parseResult
  let listSum1 = calc1 nums
  let listSum2 = calc2 nums
  putStrLn $ "part1: " ++ (show listSum1)
  putStrLn $ "part2: " ++ (show listSum2)
