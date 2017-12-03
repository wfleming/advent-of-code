module D3Lib where

import Data.List
import Data.List.Split
import Data.Maybe
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String

type Triangle = (Int, Int, Int)

valid :: Triangle -> Bool
valid (a, b, c) =
    a + b > c &&
    a + c > b &&
    b + c > a

parser :: Parser [Triangle]
parser = many parseTriangle

parseTriangle :: Parser Triangle
parseTriangle = do
    skipMany space
    v1 <- number
    skipMany space
    v2 <- number
    skipMany space
    v3 <- number
    newline
    return (v1, v2, v3)

number :: Parser Int
number = read <$> many1 digit

colTriangles :: [Triangle] -> [Triangle]
colTriangles triangles = map tFromList $ chunksOf 3 vals
  where
    tFromList [a, b, c] = (a, b, c)
    tFromList _ = error "that's not the correct size of list"
    (t1, t2, t3) = unzip3 triangles
    vals = t1 ++ t2 ++ t3
