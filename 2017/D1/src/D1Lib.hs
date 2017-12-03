module D1Lib where

import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Char
import Text.Megaparsec.Lexer as L

type ParseResult = Either (ParseError Char Dec) [Integer]

wrapl :: a -> [a]
wrapl = replicate 1

digit :: Parser Integer
digit = read <$> (wrapl <$> digitChar)

parser :: Parser [Integer]
parser = many digit

parseStr :: String -> ParseResult
parseStr = parse parser "NO_INPUT_FILE"

calc1 :: [Integer] -> Integer
calc1 = sum . map pairVal . pairs 1

calc2 :: [Integer] -> Integer
calc2 xs = sum . map pairVal $ pairs (length xs `div` 2) xs

pairs :: Int -> [a] -> [(a, a)]
pairs shiftDist xs = zip xs $ shift shiftDist xs
pairs _ [] = []

shift :: Int -> [a] -> [a]
shift d xs = t' ++ h'
  where
    (h', t') = splitAt d xs

pairVal :: (Integer, Integer) -> Integer
pairVal (x, y) = if x == y then x else 0
