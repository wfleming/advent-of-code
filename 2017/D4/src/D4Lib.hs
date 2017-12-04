module D4Lib  where

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char as C
import Data.List (nub, sort)

type ParseResult = Either (ParseError Char Dec) [Passphrase]

type Passphrase = [String]

-- using this after `sepBy` in `passphraseParser` hangs indefinitely. Why?
-- skipSpaces :: Parser ()
-- skipSpaces = skipMany (char ' ' <|> char '\t')

passphraseParser :: Parser Passphrase
passphraseParser = between
  C.space
  eol $ -- should really be eol <|> eof but that's not typechecking: skip vs return?
  (many alphaNumChar) `sepBy` (char ' ')

parser :: Parser [Passphrase]
parser = many passphraseParser

valid :: Passphrase -> Bool
valid p = length (nub p) == length p

validCount :: [Passphrase] -> Int
validCount = length . filter valid

validAnagramCount :: [Passphrase] -> Int
validAnagramCount = length . filter valid . map (map sort)

