{-# LANGUAGE BangPatterns #-}
module D18Lib  where

import Data.List
import Data.Maybe

import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq

import Debug.Trace

data Operand = Const Int | RegRef Char deriving (Eq, Show)
data Op = Snd Operand
        | Set Operand Operand
        | Add Operand Operand
        | Mul Operand Operand
        | Mod Operand Operand
        | Rcv Operand
        | Jgz Operand Operand
        deriving (Eq, Show)

data Machine = Machine
  { mOps :: Seq.Seq Op
  , mPos :: Int
  , mRegs :: Map.Map Char Int
  , mLastPlayed :: Maybe Int
  } deriving (Eq, Show)

newRegs :: Map.Map Char Int
newRegs = foldl' (\m r -> Map.insert r 0 m) Map.empty ['a'..'z']

newMachine :: [Op] -> Machine
newMachine ops = Machine { mOps = Seq.fromList ops, mPos = 0, mRegs = newRegs, mLastPlayed = Nothing }

runUntil :: (Machine -> Bool) -> Machine -> Machine
runUntil f = until (\m -> halted m || f m) step

halted :: Machine -> Bool
halted Machine { mPos = p, mOps = ops } = p >= Seq.length ops || p < 0

willRecover :: Machine -> Bool
willRecover m = case nextOp m of
  (Rcv x) -> opVal m x /= 0
  _ -> False

nextOp :: Machine -> Op
nextOp m = (mOps m) `Seq.index` (mPos m)

step :: Machine -> Machine
step m = apply m (nextOp m)

opVal :: Machine -> Operand -> Int
opVal _ (Const i) = i
opVal m (RegRef c) = regVal m c

regVal :: Machine -> Char -> Int
regVal m c = (mRegs m) Map.! c

setReg :: Machine -> Char -> Int -> Machine
setReg m c v = m { mRegs = Map.insert c v (mRegs m) }

incrPos :: Machine -> Machine
incrPos m = m { mPos = 1 + mPos m }

apply :: Machine -> Op -> Machine
apply m (Snd x) = incrPos $ m { mLastPlayed = Just (opVal m x) }
apply m (Set (RegRef r) x) = incrPos . setReg m r $ opVal m x
apply m (Add (RegRef r) x) = incrPos . setReg m r $ regVal m r + opVal m x
apply m (Mul (RegRef r) x) = incrPos . setReg m r $ regVal m r * opVal m x
apply m (Mod (RegRef r) x) = incrPos . setReg m r $ regVal m r `mod` opVal m x
apply m (Rcv _) = incrPos m -- receiving does not appear to change observable machine state
apply m (Jgz x y) = if opVal m x == 0 then incrPos m else m { mPos = opVal m y + mPos m }
apply _ op = error $ "invalid operation: " ++ show op

--- Part 2

data System = System
  { sM0 :: Machine
  , sM1 :: Machine
  , sChan0 :: Seq.Seq Int
  , sChan1 :: Seq.Seq Int
  , sSndCount0 :: Int
  , sSndCount1 :: Int
  }

newSystem :: [Op] -> System
newSystem ops = System
  { sM0 = newMachine ops
  , sM1 = setReg (newMachine ops) 'p' 1
  , sChan0 = Seq.empty -- the in channel for m0, out channel for m1
  , sChan1 = Seq.empty -- scratch that, reverse it
  , sSndCount0 = 0
  , sSndCount1 = 0
  }

step' :: Machine -> Seq.Seq Int -> (Machine, Maybe Int, Seq.Seq Int)
step' m inChan | halted m = (m, Nothing, inChan)
               | otherwise = apply' m inChan (nextOp m)

apply' :: Machine -> Seq.Seq Int -> Op -> (Machine, Maybe Int, Seq.Seq Int)
apply' m inChan (Snd x) = (incrPos m, Just (opVal m x), inChan)
apply' m inChan (Rcv (RegRef r)) = case Seq.viewl inChan of
  Seq.EmptyL -> (m, Nothing, inChan)
  (h Seq.:< t) -> (incrPos (setReg m r h), Nothing, t)
apply' m inChan op = (apply m op, Nothing, inChan)

pushMaybe :: Seq.Seq a -> Maybe a -> Seq.Seq a
pushMaybe s Nothing = s
pushMaybe s (Just x) = s Seq.|> x

stepSys :: System -> System
stepSys s = let
    (m0', sent0, chan0') = step' (sM0 s) (sChan0 s)
    (m1', sent1, chan1') = step' (sM1 s) (sChan1 s)
  in
    System { sM0 = m0' `seq` m0'
      , sM1 = m1' `seq` m1'
      , sChan0 = pushMaybe chan0' sent1
      , sChan1 = pushMaybe chan1' sent0
      , sSndCount0 = if isJust sent0 then 1 + sSndCount0 s else sSndCount0 s
      , sSndCount1 = if isJust sent0 then 1 + sSndCount1 s else sSndCount1 s
      }

waiting :: Machine -> Seq.Seq Int -> Bool
waiting m inChan = case (nextOp m, Seq.length inChan) of
  (Rcv _, 0) -> True
  _          -> False

terminated :: System -> Bool
terminated s = let
    waitingOrHalted m chan = waiting m chan || halted m
  in
    waitingOrHalted (sM0 s) (sChan0 s) &&
    waitingOrHalted (sM1 s) (sChan1 s)

runSys :: System -> System
-- runSys !s = until (\s' -> haltedSys s' || deadlocked s') stepSys s
runSys !s | terminated s = s
          | otherwise = let
              s' = stepSys s
            in
              -- trace ("DEBUG: chan0 len =" ++ ((show . length . sChan0) s') ++ " chan1 len =" ++ ((show . length . sChan1) s')) $
              runSys s'
