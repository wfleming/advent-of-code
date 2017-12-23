module Main where

import D15Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  [aSeed, bSeed] <- map read . take 2 <$> getArgs
  let genA = Gen { gSeed = aSeed, gFactor = factorA }
  let genB = Gen { gSeed = bSeed, gFactor = factorB }
  let matchCount = runJudge genA genB iterations
  putStrLn $ "p1: matchCount = " ++ (show matchCount)
