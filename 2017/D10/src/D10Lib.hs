module D10Lib  where

import Data.Bits (xor)
import Data.Char (ord)
import Data.List (foldl')
import Data.Text (strip, unpack, pack)
import Text.Printf (printf)
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

runRound :: State -> State
runRound s@(State _ [] _ _) = s
runRound s = runRound $ step s

seedBytes :: String -> [Int]
seedBytes = (++ [17, 31, 73, 47, 23]) . reverse . go []
  where
    go bytes [] = bytes
    go bytes (h : t) = go (ord h : bytes) t

runRounds :: Int -> State -> State
runRounds 0 s = s
runRounds n s@(State _ ls _ _) = runRounds (n - 1) (State ns ls pos skip)
  where
    (State ns _ pos skip) = runRound s

chunk :: Int -> [a] -> [[a]]
chunk n = reverse . go []
  where
    go memo [] = memo
    go memo xs = go ((take n xs) : memo) (drop n xs)

xorList :: [Int] -> Int
xorList [] = 0
xorList (h : t) = foldl' xor h t

compactBytes :: [Int] -> [Int]
compactBytes = map xorList . chunk 16

hash :: String -> String
hash input = foldr f "" $ compactBytes ns
  where
    f b memo = printf "%02x%s" b memo
    s0 = (new [0..255] . seedBytes . unpack . strip . pack) input
    (State ns _ _ _) = runRounds 64 s0
