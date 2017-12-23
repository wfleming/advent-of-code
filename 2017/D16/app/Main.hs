module Main where

import D16Lib
import D16Lib.Parser as P
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- P.parseFile file
  let endDancers = run dancers <$> parseResult
  putStrLn $ "p1: dancers after dance = " ++ (show endDancers)

  let c = cycleLength dancers <$> parseResult
  let realIters = (iterations `mod`) <$> c
  let endDancersP2 = runTimes dancers <$> parseResult <*> realIters
  putStrLn $ "p2: cycle length = " ++ (show c) ++ ", only doing " ++ (show realIters) ++ " instead of a billion"
  putStrLn $ "p2: dancers after dance = " ++ (show endDancersP2)
