module Main (main) where

import D13Lib
import qualified PathSearch as PS

main :: IO ()
main = do
  let start = MazeState { point = (1, 1), key = 1352, goal = (31, 39) }
  let p = PS.minPath start

  putStrLn $ "path length: " ++ show (PS.length p) ++
      " (that's " ++ show (PS.length p - 1) ++ " steps.)"

  let walkablePoints = pointsWithin start 50
  putStrLn $ "there are: " ++ show (length walkablePoints) ++
      " unique points accessible within 50 steps"

