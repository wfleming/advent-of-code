{-# LANGUAGE BangPatterns #-}
module D17Lib  where

import Data.Maybe (fromMaybe)
import Data.List (findIndex)

data Buffer = Buffer [Int] Pos deriving (Eq, Show)
type Pos = Int

-- Err: "Expected kind ‘* -> *’, but ‘Buffer’ has kind ‘*’". Wut?
-- instance Foldable Buffer where
--   foldMap f (Buffer xs p) = Buffer (foldMap f xs) p
--   foldr f memo (Buffer xs p) = Buffer (foldr f memo xs) p
--   length (Buffer xs p) = length xs

bLength (Buffer xs _) = length xs

iterationsP1 :: Int
iterationsP1 = 2017

iterationsP2 :: Int
iterationsP2 = 50000000 -- 50 million

new :: Buffer
new = Buffer [0] 0

step :: Int -> Buffer -> Buffer
step stepDist (Buffer xs p) = let
    nextV = length xs
    insertAfter = (p + stepDist) `mod` length xs
    (h, t) = splitAt (insertAfter + 1) xs
    xs' = h ++ (nextV : t)
  in
    Buffer xs' (insertAfter + 1)

-- run all 2017 iterations with a given stepDist
run :: Int -> Int -> Buffer
run iterations stepDist = until
  ((iterations <) . bLength)
  (step stepDist)
  new

bAfter :: Int -> Buffer -> Int
bAfter x (Buffer xs _) = let
    i = 1 + fromMaybe (error "2017 doesn't exist") (findIndex (== x) xs)
  in
    xs !! (i `mod` length xs)

-- part 2: discard the entire buffer except current pos/highest val/val at pos

             -- length, pos, at1
type Buffer2 = (Int, Pos, Int)

really :: a -> a
really f = f `seq` f

run2 :: Int -> Int -> Buffer2
run2 iterations stepDist =
    go (1, 0, 0)
  where
    go b@(l, _, _)
      | iterations < l = b
      | otherwise = go . really $ step2 stepDist b

step2 :: Int -> Buffer2 -> Buffer2
step2 !stepDist !(len, pos, at1) = let
    nextV = len
    insertAfter = (pos + stepDist) `mod` len
    at1' = if insertAfter == 0 then nextV else at1
  in
    (len + 1, insertAfter + 1, at1')
