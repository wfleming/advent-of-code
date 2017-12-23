module D16Lib  where

import Data.List
import Data.Maybe

data Step = Spin Int | Exchange Int Int | Partner Char Char deriving (Eq, Show)
type Dancer = Char

iterations :: Int
iterations = 1000000000 -- 1 billion

dancers :: [Dancer]
dancers = ['a'..'p']

run :: [Dancer] -> [Step] -> [Dancer]
run = foldl' step

step :: [Dancer] -> Step -> [Dancer]
step ds (Spin x) = let
    (h, t) = splitAt (length ds - x) ds
  in
    t ++ h
step ds (Exchange x y)
  | x == y = ds
  | x > y = step ds (Exchange y x)
  | otherwise = let
      (h0, (d0:t0)) = splitAt x ds
      (h1, (d1:t1)) = splitAt (y - x - 1) t0
    in
      h0 ++ (d1 : h1) ++ (d0: t1)
step ds (Partner x y) = let
    p1 = fromMaybe (error "invalid dancer id") $ findIndex (== x) ds
    p2 = fromMaybe (error "invalid dancer id") $ findIndex (== y) ds
  in
    step ds $ Exchange p1 p2

cycleLength :: [Dancer] -> [Step] -> Int
cycleLength ds ss =
    go ds 0
  where
    go ds' i
      | ds == run ds' ss = i + 1
      | otherwise = go (run ds' ss) (i + 1)

runTimes :: [Dancer] -> [Step] -> Int -> [Dancer]
runTimes ds ss is
  | is <= 0 = ds
  | otherwise = runTimes (run ds ss) ss (is - 1)
