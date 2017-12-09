module D8Lib.Data where

import qualified Data.Map.Strict as M
import Data.List

type Register = String
data Action = Inc Int | Dec Int deriving (Eq, Show)
data CmpOp = Gt | Gte | Lt | Lte | Eq | Neq deriving (Eq, Show)
data Cmp = Cmp Register CmpOp Int deriving (Eq, Show)
data Rule = Rule Register Action Cmp deriving (Eq, Show)
type Registers = M.Map Register Int

registerNames :: [Rule] -> [Register]
registerNames = map (\(Rule r _ _) -> r)

initState :: [Rule] -> Registers
initState =
    foldl' ins M.empty . registerNames
  where
    ins m r = M.insert r 0 m

apply :: Registers -> Rule -> Registers
apply regs (Rule r a c)
    | testCnd c regs = M.update (fn a) r regs
    | otherwise = regs
  where
    fn (Inc x) = Just . (+ x)
    fn (Dec x) = Just . (\y -> y - x)

applyAll :: Registers -> [Rule] -> Registers
applyAll = foldl' apply

testCnd :: Cmp -> Registers -> Bool
testCnd (Cmp r op v) regs =
    (fn op) rVal v
  where
    rVal :: Int
    rVal = maybe 0 id $ M.lookup r regs
    fn :: CmpOp -> (Int -> Int -> Bool)
    fn Gt = (>)
    fn Gte = (>=)
    fn Lt = (<)
    fn Lte = (<=)
    fn Eq = (==)
    fn Neq = (/=)
