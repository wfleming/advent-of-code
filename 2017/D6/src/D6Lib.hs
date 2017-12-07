module D6Lib  where

import Data.List
import Data.Maybe
import Text.Megaparsec (ParseError, Dec, many)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Lexer as L

type ParseResult = Either (ParseError Char Dec) Mem

type Mem = [Int]

parser :: Parser Mem
parser = many $ L.lexeme C.space pInt
  where
    pInt = fromIntegral <$> L.integer :: Parser Int

reallocate :: Mem -> Mem
reallocate m =
    distribute m' maxVal (maxIdx + 1)
  where
    maxVal = maximum m
    maxIdx = fromJust $ elemIndex maxVal m
    (h, t) = splitAt maxIdx m
    m' = h ++ (0 : tail t)

distribute :: Mem -> Int -> Int -> Mem
distribute m 0 _ = m
distribute m v idx
    | idx >= length m = distribute m v 0
    | True            = distribute m' (v - 1) (idx + 1)
  where
    (h, t) = splitAt idx m
    m' = h ++ (1 + head t : tail t)

reallocateUntilDupe :: [Mem] -> [Mem]
reallocateUntilDupe [] = error "don't send a non-empty array"
reallocateUntilDupe (h : t) = case find (== h) t of
  Just _  -> h : t
  Nothing -> reallocateUntilDupe $ (reallocate h) : h : t

-- how many steps between head of list & the previous time the same state was
-- seen
cycleLength :: [Mem] -> Maybe Int
cycleLength [] = Nothing
cycleLength (h : t) = (+ 1) <$> elemIndex h t
