module D10Lib  where

import Text.Megaparsec (parse, sepBy, char, eol, ParseError, Dec)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Lexer as L

type ParseResult = Either (ParseError Char Dec) [Int]

             -- numbers list, lengths list, position, skip size
data State = State [Int] [Int] Int Int deriving (Eq, Show)

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

new :: [Int] -> [Int] -> State
new ns lengths = State ns lengths 0 0

step :: State -> State
step s@(State _ [] _ _) = s
step (State ns (lsH : lsT) pos skip) =
    State ns' lsT pos' (skip + 1)
  where
    pos' = (pos + lsH + skip) `mod` (length ns)
    (nsH, nsT) = splitAt pos ns
    nsTmp0 = nsT ++ nsH
    nsTmp1 = (reverse . take lsH) nsTmp0 ++ drop lsH nsTmp0
    (nsH1, nsT1) = splitAt (length ns - pos) nsTmp1
    ns' = nsT1 ++ nsH1

run :: State -> State
run s@(State _ [] _ _) = s
run s = run $ step s
