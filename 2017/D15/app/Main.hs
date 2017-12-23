module Main where

import D15Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  [aSeed, bSeed] <- map read . take 2 <$> getArgs

  let genA = Gen { gSeed = aSeed, gFactor = factorA, gModuloGate = 1 }
  let genB = Gen { gSeed = bSeed, gFactor = factorB, gModuloGate = 1 }
  let matchCount = runJudge genA genB iterationsP1
  putStrLn $ "p1: matchCount = " ++ (show matchCount)

  let genAP2 = Gen { gSeed = aSeed, gFactor = factorA, gModuloGate = 4 }
  let genBP2 = Gen { gSeed = bSeed, gFactor = factorB, gModuloGate = 8 }
  let matchCountP2 = runJudge genAP2 genBP2 iterationsP2
  putStrLn $ "p2: matchCount = " ++ (show matchCountP2)

  -- let genA = Gen { gSeed = aSeed, gFactor = factorA, gModuloGate = 1 }
  -- let genB = Gen { gSeed = bSeed, gFactor = factorB, gModuloGate = 1 }
  -- genA' <- wrapGen genA
  -- genB' <- wrapGen genB
  -- -- lower iteration count for benchmarking
  -- matchCount <- runJudgePar genA' genB' 1000000 --iterationsP1
  -- putStrLn $ "p1: matchCount = " ++ (show matchCount)

  -- let genAP2 = Gen { gSeed = aSeed, gFactor = factorA, gModuloGate = 4 }
  -- let genBP2 = Gen { gSeed = bSeed, gFactor = factorB, gModuloGate = 8 }
  -- genAP2' <- wrapGen genAP2
  -- genBP2' <- wrapGen genBP2
  -- matchCountP2 <- runJudgePar genAP2' genBP2' iterationsP2
  -- putStrLn $ "p2: matchCount = " ++ (show matchCountP2)
