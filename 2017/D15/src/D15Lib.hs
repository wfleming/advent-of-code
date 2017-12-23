module D15Lib  where

import Data.Bits

data Gen = Gen { gSeed :: Integer , gFactor :: Integer } deriving (Eq, Show)

divisor :: Integer
divisor = 2147483647

factorA :: Integer
factorA = 16807

factorB :: Integer
factorB = 48271

iterations = 40000000 -- 40 million

next :: Gen -> (Gen, Integer)
next gen =
  let
    val = (gSeed gen * gFactor gen) `mod` divisor
  in
    ( Gen { gSeed = val, gFactor = gFactor gen }
    , val
    )

lpad :: Int -> Char -> String -> String
lpad l c s = replicate (l - length s) c ++ s

-- compare the lower 16 bits for equality
matches :: Integer -> Integer -> Bool
matches x y = let
    iVal i = let f = fromInteger :: Integer -> Int
           in if f i < 0 then error "underflow" else f i
    shiftSize = finiteBitSize (0 :: Int) - 16
    -- I prototyped with string comparison first: unsurprisingly,it was fastly
    -- slower than bit-shifting
    -- f = lpad 16 '0' . take 16 . reverse . printf "%b"
    f i = shiftL (iVal i) shiftSize :: Int
  in
    (f x) == (f y)

-- run a certain number of iterations, report how many matched
runJudge :: Gen -> Gen -> Integer -> Integer
runJudge ga gb iters
  | iters <= 0 = 0
  | otherwise =
    let
      (ga', va) = next ga
      (gb', vb) = next gb
      m = if matches va vb then 1 else 0
    in
      m + runJudge ga' gb' (iters - 1)
