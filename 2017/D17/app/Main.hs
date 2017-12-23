module Main where

import D17Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  stepDist <- read . head <$> getArgs
  let bP1 = run iterationsP1 stepDist
  let p1 = bAfter iterationsP1 bP1
  putStrLn $ "p1 after 2017 = " ++ (show p1)

  -- let (_, _, at1) = run2 iterationsP2 stepDist
  -- putStrLn $ "p2 after 0 = " ++ (show at1)
