module D12Lib where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Text.Parsec hiding (Error)
import Text.Parsec.String

type Register = (Char, Int)

data RegOperand = Reg Char deriving (Eq, Show)
type RegOrIntOperand = Either RegOperand Int

data Instruction =
    Cpy RegOrIntOperand RegOperand |
    Jnz RegOrIntOperand Int |
    Inc RegOperand |
    Dec RegOperand
    deriving (Eq, Show)

data VM = VM
    { registers :: [Register]
    , tape :: [Instruction]
    , pos :: Int
    } deriving (Eq, Show)

instructionsP :: Parser [Instruction]
instructionsP = instructionP `sepEndBy1` newline

instructionP :: Parser Instruction
instructionP = choice $ map try [cpyP, incP, decP, jnzP]

cpyP :: Parser Instruction
cpyP = do
    void $ string "cpy "
    operand1 <- eitherP registerP numberP
    void $ space
    operand2 <- registerP
    return $ Cpy operand1 operand2

incP :: Parser Instruction
incP = do
    void $ string "inc "
    operand1 <- registerP
    return $ Inc operand1

decP :: Parser Instruction
decP = do
    void $ string "dec "
    operand1 <- registerP
    return $ Dec operand1

jnzP :: Parser Instruction
jnzP = do
    void $ string "jnz "
    operand1 <- eitherP registerP numberP
    void $ space
    operand2 <- numberP
    return $ Jnz operand1 operand2

eitherP :: Parser a -> Parser b -> Parser (Either a b)
eitherP pl pr = (Left <$> pl) <|> (Right <$> pr)

registerP :: Parser RegOperand
registerP = Reg <$> lower

numberP :: Parser Int
numberP = read <$> (negNumber <|> number)
  where
    negNumber = (:) <$> char '-' <*> number
    number = many1 digit

terminated :: VM -> Bool
terminated VM { tape = ops, pos = p } = p >= length ops

newVM :: [Instruction] -> VM
newVM ops = VM
    { registers = [('a', 0), ('b', 0), ('c', 0), ('d', 0)]
    , tape = ops
    , pos = 0
    }

newVM2 :: [Instruction] -> VM
newVM2 ops = VM
    { registers = [('a', 0), ('b', 0), ('c', 1), ('d', 0)]
    , tape = ops
    , pos = 0
    }

step :: VM -> VM
step vm@VM { registers = rs, tape = ops, pos = p } = apply vm (ops !! p)

apply :: VM -> Instruction -> VM
apply vm (Cpy (Left r0) r1) = -- read r0, write to r1, new vm, move 1
    move (set vm r1 (get vm r0)) 1
apply vm (Cpy (Right v) r) = -- write c to r, new vm, move 1
    move (set vm r v) 1
apply vm (Inc r) = -- increment r, move 1
    move (set vm r ((get vm r) + 1)) 1
apply vm (Dec r) = -- decrement r, move 1
    move (set vm r ((get vm r) - 1)) 1
apply vm (Jnz (Left r) c) = apply vm (Jnz (Right (get vm r)) c)
apply vm (Jnz (Right c0) c1) = -- if c0 == 0, move c1, otherwise move 1
    if c0 /= 0
    then move vm c1
    else move vm 1

get :: VM -> RegOperand -> Int
get vm (Reg r) = fromMaybe (error "bad register") $ lookup r (registers vm)

set :: VM -> RegOperand -> Int -> VM
set vm (Reg r) v = VM
    { registers = (map setReg . registers) vm
    , tape = tape vm
    , pos = pos vm }
  where
    setReg (rc, rv) | rc == r = (rc, v)
    setReg (rc, rv) = (rc, rv)

{- | move tape position -}
move :: VM -> Int -> VM
move VM { registers = rs, tape = t, pos = p } c = VM
    { registers = rs
    , tape = t
    , pos = p + c
    }

run :: VM -> VM
run = until terminated step
