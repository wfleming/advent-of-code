module Main where

import D14Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- head <$> getArgs
  let g = grid input
  let usedCellCount = used g
  putStrLn $ "p1: used cells count = " ++ show usedCellCount
  let grps = groups g
  putStrLn $ "p2: number of regions = " ++ show (length grps)
