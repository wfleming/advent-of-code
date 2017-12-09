module D8Lib.Parser where

import Text.Megaparsec
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Lexer as L

import D8Lib.Data

type ParseResult = Either (ParseError Char Dec) [Rule]

register :: Parser Register
register = many letterChar

int :: Parser Int
int = L.signed C.space (fromIntegral <$> L.integer)

action :: Parser Action
action = do
  opCtor <- (string "inc" >> pure Inc) <|> (string "dec" >> pure Dec)
  char ' '
  v <- int
  return $ opCtor v

cmpOp :: Parser CmpOp
cmpOp = choice
  [ string ">=" >> pure Gte
  , string ">" >> pure Gt
  , string "<=" >> pure Lte
  , string "<" >> pure Lt
  , string "==" >> pure Eq
  , string "!=" >> pure Neq
  ]

comparison :: Parser Cmp
comparison = do
  r <- register
  char ' '
  op <- cmpOp
  char ' '
  v <- int
  return $ Cmp r op v

rule :: Parser Rule
rule = do
  r <- register
  char ' '
  a <- action
  string " if "
  c <- comparison
  eol
  return $ Rule r a c

rules :: Parser [Rule]
rules = many rule
