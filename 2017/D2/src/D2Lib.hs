module D2Lib  where

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char as C
import Text.Megaparsec.Lexer as L
import Data.List (find, sort)

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

checksum2 :: Sheet -> Integer
checksum2 = sum . map rowsum2

rowsum2 :: Row -> Integer
rowsum2 r = high `div` low
  where
    (high, low) = rowfactor r

rowfactor :: Row -> (Integer, Integer)
rowfactor xs =
    case (find isFactor candidates) of
      Just (h, l) -> (h, l)
      Nothing -> (0, 0)
  where
    candidates = pairs xs
    isFactor (x, y) = x `mod` y == 0

pairs :: Row -> [(Integer, Integer)]
pairs =
    chunk . reverse . sort
  where
    chunk :: [Integer] -> [(Integer, Integer)]
    chunk (h : t) =  (zip (repeat h) t) ++ chunk t
    chunk [] = []
