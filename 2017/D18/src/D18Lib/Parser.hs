module D18Lib.Parser where

import D18Lib (Op(..), Operand(..))

import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParseResult = Either (ParseError Char Void) [Op]

parseFile :: String -> IO ParseResult
parseFile path = do
  contents <- readFile path
  return $ parse ops path contents

ws :: Parser ()
ws = skipMany $ char ' '

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

op :: Parser Op
op = unaryOp <|> binaryOp

unaryOp :: Parser Op
unaryOp = unaryOpCtor <*> operand

binaryOp :: Parser Op
binaryOp = binaryOpCtor <*> operand <*> operand

unaryOpCtor :: Parser (Operand -> Op)
unaryOpCtor = do
  n <- lexeme $ string "snd" <|> string "rcv"
  return $ case n of
    "snd" -> Snd
    "rcv" -> Rcv
    _ -> error "invalid op how did that happen"

binaryOpCtor :: Parser (Operand -> Operand -> Op)
binaryOpCtor = do
  n <- lexeme $ choice [string "set", string "add", string "mul", string "mod", string "jgz" ]
  return $ case n of
    "set" -> Set
    "add" -> Add
    "mul" -> Mul
    "mod" -> Mod
    "jgz" -> Jgz
    _ -> error "invalid op how did that happen"

operand :: Parser Operand
operand = lexeme $ intOperand <|> regOperand

intOperand :: Parser Operand
intOperand = Const <$> (L.signed ws L.decimal)

regOperand :: Parser Operand
regOperand = RegRef <$> lowerChar

ops :: Parser [Op]
ops = op `endBy` eol
