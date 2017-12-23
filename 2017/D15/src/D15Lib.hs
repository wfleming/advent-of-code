module D15Lib  where

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Async (async)
import Data.Bits

data Gen = Gen
  { gSeed :: Int
  , gFactor :: Int
  , gModuloGate :: Int
  } deriving (Eq, Show)

data ParGen = ParGen Gen (Chan (ParGen, Int))

divisor :: Int
divisor = 2147483647

factorA :: Int
factorA = 16807

factorB :: Int
factorB = 48271

iterationsP1 :: Int
iterationsP1 = 40000000 -- 40 million

iterationsP2 :: Int
iterationsP2 = 5000000 -- 5 million

next :: Gen -> (Gen, Int)
next gen =
  let
    val = (gSeed gen * gFactor gen) `mod` divisor
    gen' = Gen { gSeed = val, gFactor = gFactor gen, gModuloGate = gModuloGate gen }
  in
    if val `mod` gModuloGate gen == 0
    then (gen', val)
    else next gen'

wrapGen :: Gen -> IO ParGen
wrapGen gen = do
  c <- newChan
  return $ ParGen gen c

nextPar :: ParGen -> IO ()
nextPar (ParGen gen chan) = do
  let (gen', v) = next gen
  writeChan chan (ParGen gen' chan, v)

genChan :: ParGen -> Chan (ParGen, Int)
genChan (ParGen _ c) = c

lpad :: Int -> Char -> String -> String
lpad l c s = replicate (l - length s) c ++ s

-- compare the lower 16 bits for equality
matches :: Int -> Int -> Bool
matches x y = let
    shiftSize = finiteBitSize x - 16
    -- I prototyped with string comparison first: unsurprisingly, it was vastly
    -- slower than bit-shifting
    -- f = lpad 16 '0' . take 16 . reverse . printf "%b"
    f i = shiftL i shiftSize :: Int
  in
    (f x) == (f y)

-- run a certain number of iterations, report how many matched
runJudge :: Gen -> Gen -> Int -> Int
runJudge ga gb iters
  | iters <= 0 = 0
  | otherwise =
    let
      (ga', va) = next ga
      (gb', vb) = next gb
      m = if matches va vb then 1 else 0
    in
      m + runJudge ga' gb' (iters - 1)

runJudgePar :: ParGen -> ParGen -> Int -> IO Int
runJudgePar ga gb iters
  | iters <= 0 = return 0
  | otherwise = do
    async $ nextPar ga
    async $ nextPar gb
    (ga', va) <- readChan $ genChan ga
    (gb', vb) <- readChan $ genChan gb
    let m = if matches va vb then 1 else 0
    (m +) <$> runJudgePar ga' gb' (iters - 1)

