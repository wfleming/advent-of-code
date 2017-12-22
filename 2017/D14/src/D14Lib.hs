module D14Lib  where

import Data.Char (digitToInt)
import Data.List -- (any, foldl', groupBy, intersect, nub, union)
import Data.Maybe
import Text.Printf (printf)
import Data.Bits (popCountDefault)

import qualified Data.Digest.Knot as K

-- an array of 0s and 1s
newtype Row = Row [Int]
type Grid = [Row]
type Pos = (Int, Int) -- row,col
type Group = [Pos]

rowData :: Row -> [Int]
rowData (Row xs) = xs

instance Show Row where
  show (Row xs) = concatMap show xs

row :: String -> Int -> Row
row s n = Row . hexBits . K.hash $ s ++ "-" ++ show n

-- convert hex to bit integers
hexBits :: String -> [Int]
hexBits = map digitToInt . concatMap (printf "%04b" . digitToInt)

grid :: String -> Grid
grid s = map (row s) [0..127]

rowUsed :: Row -> Int
rowUsed (Row xs) = sum xs

used :: Grid -> Int
used = foldl' (\m r -> m + rowUsed r) 0

gridWidth :: Grid -> Int
gridWidth = length . rowData . head

gridHeight :: Grid -> Int
gridHeight = length

gridRow :: Grid -> Int -> Row
gridRow g r = g !! r

rowSpans :: Grid -> Int -> [Group]
rowSpans g rIdx = map (map fst) filled
  where
    coords = zip (repeat rIdx) [0..(gridWidth g - 1)]
    vals = zip coords . rowData . gridRow g $ rIdx
    runs = groupBy sameVal vals
    sameVal (_, v1) (_, v2) = v1 == v2
    filled = filter (\((_, v):_) -> 1 == v) runs

spanOverlap :: Group -> Group -> Bool
spanOverlap grp1 grp2 =
    any (\(r, c) -> isJust (find (adjacent (r, c)) grp2)) grp1
  where
    adjacent (r1, c1) (r2, c2) = c1 == c2 && abs(r2 - r1) == 1

mergeSpans :: [Group] -> Group -> [Group]
mergeSpans (h:t) grp =
    if spanOverlap h grp then union h grp : t
               else h : mergeSpans t grp
mergeSpans [] grp = [grp]

groups :: Grid -> [Group]
groups g =
    minGroups $ foldl' go [] [0..(gridHeight g - 1)]
  where
    go grps r = foldl' mergeSpans grps $ rowSpans g r

minGroups :: [Group] -> [Group]
minGroups = until groupsAreCompact (foldl' mergeSpans [])

groupsAreCompact :: [Group] -> Bool
groupsAreCompact [] = True
groupsAreCompact (h:t) | any (spanOverlap h) t = False
                       | otherwise             = groupsAreCompact t

-- show the full grid with groups marked by number
showGroups :: Grid -> String
showGroups g = unlines $ map showR [0..(gridHeight g - 1)]
  where
    grps = groups g
    showR r = concatMap (maybe "." (show . (1 +)). (grpIdx r)) [0..(gridWidth g - 1)]
    grpIdx r c  = findIndex (isJust . find (== (r, c))) grps
