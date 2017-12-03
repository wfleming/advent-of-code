module D3Lib  where

import Data.List (find)
import Data.Maybe (fromJust)

type Pos = (Int, Int)

data Dir = U | D | R | L deriving (Eq, Show)

dist :: Pos -> Pos -> Int
dist (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

move :: Pos -> Dir -> Pos
move (x, y) U = (x, y + 1)
move (x, y) D = (x, y - 1)
move (x, y) R = (x + 1, y)
move (x, y) L = (x - 1, y)

cellPos :: Int -> Pos
cellPos n = walk (0, 0) (n - 1) 1 1 0 R

{-- Args:
    - current position
    - number of total steps left to take
    - counter of steps in current directions, counting down
    - number of total steps to be taken in current direction
    - number of turns taken so far
    - direction walking right now
    Returns: the sequence of steps taken
--}
walk :: Pos -> Int -> Int -> Int -> Int -> Dir -> Pos
-- base case: 0 steps left, returns steps taken so far
walk pos 0 _ _ _ _ = pos
-- Turn case: don't actually take a step, just turn & recurse
walk pos stepsN 0 ceiling turns dir =
    walk pos stepsN ceiling' ceiling' turns' dir'
  where
    turns' = turns + 1
    ceiling' = if turns' `mod` 2 == 0 then ceiling + 1 else ceiling
    dir' = turnDir dir
-- Normal walking: step forward, recurse
walk pos stepsN dirLeft ceiling turns dir =
    walk (move pos dir) (stepsN - 1) (dirLeft - 1) ceiling turns dir

turnDir :: Dir -> Dir
turnDir R = U
turnDir U = L
turnDir L = D
turnDir D = R

neighbors :: Pos -> Pos -> Bool
neighbors (x1, y1) (x2, y2) = (abs (x2 - x1) <= 1 && abs(y2 - y1) <= 1)

-- neighbors of a cell (only the ones with lower indexes)
cellNeighbors :: Int -> [Int]
cellNeighbors x =
    filter isMatch [1..(x - 1)]
  where
    isMatch y = neighbors (cellPos x) (cellPos y)

-- determie the value filled in cell n during the stress test
stressTestVal :: Int -> Int
stressTestVal 1 = 1
stressTestVal n = sum . (map stressTestVal) $ cellNeighbors n

-- find the first cell with a stress test value stored higher than the target
-- index
--
-- We're finding in an infinite list, so `find` will never return `Nothing`: if
-- mess up, we're searching infinitely.
firstValHigherThan :: Int -> Int
firstValHigherThan n = fromJust $ find (\x -> stressTestVal x > n) [1..]
