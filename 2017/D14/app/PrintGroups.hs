module Main where

import D14Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  input <- head <$> getArgs
  let g = grid input
  putStrLn $ showGroups g
