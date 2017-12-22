module D10Lib where

import Text.Megaparsec (parse, sepBy, char, eol, ParseError, Dec)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Lexer as L

type ParseResult = Either (ParseError Char Dec) [Int]

parseFile :: Parser [Int] -> String -> IO ParseResult
parseFile p path = do
  contents <- readFile path
  return $ parse p path contents

parser :: Parser [Int]
parser = do
  ints <- parseInt `sepBy` char ','
  eol
  return ints

parseInt :: Parser Int
parseInt = fromIntegral <$> L.integer

