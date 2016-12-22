module D13Lib where

import Data.List
import qualified PathSearch as PS

type Point = (Int, Int)

data MazeState = MazeState { point :: Point, key :: Int, goal :: Point }
    deriving (Eq, Show)

num2bin :: Int -> String
num2bin n
  | n >= 0     =  concatMap show $ reverse $ n2b n
  | otherwise  =  error "num2bin: negative number"
  where n2b 0  =  []
        n2b n  =  n `mod` 2 : n2b (n `div` 2)

open :: MazeState -> Bool
open MazeState { point = (x, y), key = k } =
    (even . length . filter (=='1') . num2bin) v
  where
    v = k + (x * x) + (3 * x) + (2 * x * y) + y + (y * y)

valid :: MazeState -> Bool
valid ms@MazeState { point = (x, y) } = x >= 0 && y >= 0 && open ms

instance PS.PathState MazeState where
    nextStates MazeState { point = (x, y), key = k, goal = g } = filter valid $
        [ MazeState { point = (x + 1, y), key = k, goal = g }
        , MazeState { point = (x, y + 1), key = k, goal = g }
        , MazeState { point = (x - 1, y), key = k, goal = g }
        , MazeState { point = (x, y - 1), key = k, goal = g }
        ]

    isGoal MazeState { point = p, goal = g } = p == g

    goalDist MazeState { point = (x1, y1), goal = (x2, y2) } =
        abs(x2 - x1) + abs(y2 - y1)

pointsWithin :: MazeState -> Int -> [MazeState]
pointsWithin ms maxSteps = nub $ pw [ms] maxSteps
  where
    pw memo steps
      | steps <= 0 = memo
      | otherwise = pw (nub $ memo ++ concatMap PS.nextStates memo) (steps - 1)
