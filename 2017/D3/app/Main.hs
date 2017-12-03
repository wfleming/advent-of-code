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
