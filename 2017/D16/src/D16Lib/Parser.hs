module D16Lib.Parser where

import D16Lib (Step(..))

import Data.Void
import Text.Megaparsec (choice, parse, sepBy, Parsec, ParseError)
import Text.Megaparsec.Char (char, eol, letterChar)
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParseResult = Either (ParseError Char Void) [Step]

parseFile :: String -> IO ParseResult
parseFile path = do
  contents <- readFile path
  return $ parse steps path contents

spin :: Parser Step
spin = do
  _ <- char 's'
  amt <- L.decimal
  return $ Spin amt

exchange :: Parser Step
exchange = do
  _ <- char 'x'
  p1 <- L.decimal
  _ <- char '/'
  p2 <- L.decimal
  return $ Exchange p1 p2

partner :: Parser Step
partner = do
  _ <- char 'p'
  d1 <- letterChar
  _ <- char '/'
  d2 <- letterChar
  return $ Partner d1 d2

step :: Parser Step
step = choice [spin, exchange, partner]

steps :: Parser [Step]
steps = do
  ss <- step `sepBy` char ','
  _ <- eol
  return ss
