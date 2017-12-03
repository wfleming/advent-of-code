{-# LANGUAGE DeriveGeneric #-}
module D22Lib.Node where

{-import Debug.Trace-}

import Data.Hashable (Hashable)
import GHC.Generics (Generic)

type Pos = (Int, Int) -- x, y
type Move = (Pos, Pos) -- from, to
type NodeSizes = (Int, Int) -- capacity, used

data NodeKind = Empty | Avail | Wall | Data deriving (Eq, Show, Generic)
data Node a = Node
    { pos :: Pos
    , info :: a
    } deriving (Eq, Show, Generic)

instance Hashable NodeKind
instance Hashable a => Hashable (Node a)

used :: Node NodeSizes -> Int
used Node { info = (_, u) } = u

capacity :: Node NodeSizes -> Int
capacity Node { info = (c, _) } = c

free :: Node NodeSizes -> Int
free n = capacity n - used n

-- All pairs of distinct nodes
allPairs :: [Node a] -> [(Node a, Node a)]
allPairs ns = filter notEq $ concatMap (\n -> zip ns (repeat n)) ns
  where
    notEq (n1, n2) = (pos n1) /= (pos n2)

-- all pairs where (used n1 < free n2)
viablePairs :: [Node NodeSizes] -> [(Node NodeSizes, Node NodeSizes)]
viablePairs = filter viable . allPairs
  where
    viable (n1, n2) = used n1 > 0 && used n1 <= free n2

-- All moves that can be made given a set of nodes
viableMoves :: [Node NodeKind] -> [Move]
viableMoves =
    map (\(n1, n2) -> (pos n1, pos n2)) . filter f . allPairs
  where
    f (n1, n2) =
        areNeighbors n1 n2 &&
        (info n2) == Empty &&
        (info n1) /= Wall

areNeighbors :: Node a -> Node a -> Bool
{-areNeighors n0 n1 = 1 == distance n0 n1-}
areNeighbors Node { pos = (x0, y0) } Node { pos = (x1, y1) } =
    (x0 `nextTo` x1 && y0 == y1) || (y0 `nextTo` y1 && x0 == x1)
  where
    nextTo n0 n1 = n0 + 1 == n1 || n0 - 1 == n1

-- manhattan distance between two nodes
distance :: Node a -> Node a -> Int
distance Node { pos = (x0, y0) } Node { pos = (x1, y1) } =
  abs (x1 - x0) + abs (y1 - y0)

-- if a node's used is > all neighbors capacity, it's a wall
isWall :: [Node NodeSizes] -> Node NodeSizes -> Bool
isWall ns n = all (\n' -> capacity n' < used n) neighbors
  where
    neighbors = map snd $ filter f (allPairs ns)
    f (n1, n2) = n1 == n && areNeighbors n1 n2

simplify :: [Node NodeSizes] -> [Node NodeKind]
simplify ns = map f ns
  where
    f n = Node { pos = (pos n), info = detectKind n }
    detectKind n | pos n == (maxX, 0) = Data
                 | used n == 0 = Empty
                 | isWall ns n = Wall
                 | otherwise = Avail
    maxX = maximum $ map (fst . pos) ns
