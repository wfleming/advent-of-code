module Main where

import D3Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  cellId <- read . head <$> getArgs :: IO Int
  let pos1 = cellPos cellId
  putStrLn $ "cell n=" ++ (show cellId) ++ " is at " ++ (show pos1)
  let d1 = dist (0, 0) pos1
  putStrLn $ "p1: distance to cell 1 at (0,0) = " ++ (show d1)
  let cellId2 = firstValHigherThan cellId
  putStrLn $ "p2: id of cell with higher value = " ++ (show cellId2)
  let val2 = stressTestVal  cellId2
  putStrLn $ "p2: value written to that cell= " ++ (show val2)
