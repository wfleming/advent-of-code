module D18Lib  where

import Data.Maybe
import Data.List

data Reg = Reg { rId :: Char, rVal :: Int } deriving (Eq, Show)
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
  { mOps :: [Op]
  , mPos :: Int
  , mRegs :: [Reg]
  , mLastPlayed :: Maybe Int
  } deriving (Eq, Show)

newRegs :: [Reg]
newRegs = map (\c -> Reg { rId = c, rVal = 0 }) ['a'..'z']

newMachine :: [Op] -> Machine
newMachine ops = Machine { mOps = ops, mPos = 0, mRegs = newRegs, mLastPlayed = Nothing }

runUntil :: (Machine -> Bool) -> Machine -> Machine
runUntil f = until (\m -> halted m || f m) step

halted :: Machine -> Bool
halted Machine { mPos = p, mOps = ops } = p >= length ops || p < 0

willRecover :: Machine -> Bool
willRecover m = case nextOp m of
  (Rcv x) -> opVal m x /= 0
  _ -> False

nextOp :: Machine -> Op
nextOp m = (mOps m) !! (mPos m)

step :: Machine -> Machine
step m = apply m (nextOp m)

opVal :: Machine -> Operand -> Int
opVal _ (Const i) = i
opVal m (RegRef c) = regVal m c

regVal :: Machine -> Char -> Int
regVal m c = rVal . fromJust . find ((== c) . rId) $ mRegs m

setReg :: Machine -> Char -> Int -> Machine
setReg m c v = let
    idx = fromJust . findIndex ((== c) . rId) $ mRegs m
    (h, t) = splitAt idx $ mRegs m
    regs' = h ++ (Reg { rId = c, rVal =v } : tail t)
  in
    m { mRegs = regs' }

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
