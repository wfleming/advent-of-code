module Main where

import D8Lib
import D8Lib.Data
import qualified D8Lib.Parser as P
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile P.rules file
  let rules = either (error . show) id parseResult
  let state = initState rules
  let state' = applyAll state rules
  let highestVal = largestRegVal state'
  putStrLn $ "p1: highest reg val = " ++ (show highestVal)
  let highestEver = largestEver state rules
  putStrLn $ "p2: highest reg val ever = " ++ (show highestEver)
