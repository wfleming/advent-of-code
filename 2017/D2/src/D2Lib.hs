module D2Lib  where

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char as C
import Text.Megaparsec.Lexer as L

type Row = [Integer]
type Sheet = [Row]

type ParseResult = Either (ParseError Char Dec) Sheet

skipSpaces :: Parser ()
skipSpaces = skipMany (char ' ' <|> char '\t')

rowParser :: Parser Row
rowParser = between
  C.space
  eol
  $ many (L.lexeme skipSpaces L.integer)

sheetParser :: Parser Sheet
sheetParser = many rowParser

checksum1 :: Sheet -> Integer
checksum1 = sum . map rowsum1

rowsum1 :: Row -> Integer
rowsum1 r = (maximum r) - (minimum r)
