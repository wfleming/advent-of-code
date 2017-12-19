module D11Lib  where

import Data.List
import Text.Megaparsec (parse, sepBy, char, string, eol, ParseError, Dec, choice)
import Text.Megaparsec.String (Parser)

{- The grid here is a hexagonal grid, coords work like:

  "z-aligned axial coords" as described at https://www.redblobgames.com/grids/hexagons/
-}

type Pos = (Int, Int)
type CubePos = (Int, Int, Int)
data Dir = NW | N | NE | SE | S | SW deriving (Eq, Show)

type ParseResult = Either (ParseError Char Dec) [Dir]

parseFile :: String -> IO ParseResult
parseFile path = do
  contents <- readFile path
  return $ parse parser path contents

parser :: Parser [Dir]
parser = do
  r <- parseDir `sepBy` char ','
  eol
  return r

parseDir :: Parser Dir
parseDir =
  dir <$> choice
            [ string "nw"
            , string "ne"
            , string "n"
            , string "se"
            , string "sw"
            , string "s"
            ]

dir :: String -> Dir
dir "nw" = NW
dir "n" = N
dir "ne" = NE
dir "se" = SE
dir "s" = S
dir "sw" = SW

dirVec :: Dir -> Pos
dirVec NW = (-1, 0)
dirVec N = (0, -1)
dirVec NE = (1, -1)
dirVec SE = (1, 0)
dirVec S = (0, 1)
dirVec SW = (-1, 1)

moveAll :: Pos -> [Dir] -> Pos
moveAll = foldl' move

move :: Pos -> Dir -> Pos
move (q, r) d =
    (q + dq, r + dr)
  where
    (dq, dr) = dirVec d

cube :: Pos -> CubePos
cube (q, r) = (q, (0 - q - r), r)

-- Works by converting axial coordinates above into cube coords
dist :: Pos -> Pos -> Int
dist p1 p2 =
    (abs (x1 - x2) + abs (y1 - y2) + abs (z1 - z2)) `div` 2
  where
    (x1, y1, z1) = cube p1
    (x2, y2, z2) = cube p2

furthestDistWalked :: Pos -> [Dir] -> Int
furthestDistWalked p =
    snd . foldl' step (p, 0)
  where
    step (p', maxd) d = (move p' d, max maxd (dist p (move p' d)))
