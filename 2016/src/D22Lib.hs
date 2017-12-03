{-# LANGUAGE DeriveGeneric #-}
module D22Lib (
  Node(..),
  NodeKind(..),
  NodesState(..),
  Pos,
  capacity,
  distance,
  findPath,
  get,
  move,
  nodesP,
  simplify,
  sourceNode,
  used,
  viableMoves,
  viablePairs
) where

import Debug.Trace

import AStar
import D22Lib.Node
import D22Lib.Parser
import Data.List (find)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import GHC.Generics (Generic)

data NodesState = NodesState
    { nodes :: [Node NodeKind]
    } deriving (Eq, Generic)

instance Hashable NodesState

-- The node at (x = max x, y = 0)
sourceNode :: [Node a] -> Node a
sourceNode [n] = n
sourceNode (n0:n1:t) = case pos n0 of
  (x0, 0) -> let (x1, y1) = pos n1 in
      if 0 == y1 && x1 > x0
      then sourceNode (n1:t)
      else sourceNode (n0:t)
  _ -> sourceNode (n1:t)

emptyNode :: [Node NodeKind] -> Node NodeKind
emptyNode = fromMaybe (error "no empty node") . find ((Empty ==) . info)

dataNode :: [Node NodeKind] -> Node NodeKind
dataNode = fromMaybe (error "no data node") . find ((Data ==) . info)

get :: Pos -> [Node a] -> Node a
get p = fromMaybe (error "no such coord") . find ((p==) . pos)

replace :: Node a -> [Node a] -> [Node a]
replace _ [] = []
replace n (h:t) = if pos h == pos n
    then n:t
    else h : replace n t

move :: Pos -> Pos -> [Node a] -> [Node a]
move p0 p1 ns = replace n1' $ replace n0' ns
  where
    n0 = get p0 ns
    n1 = get p1 ns
    n0' = Node { pos = pos n0, info = info n1 }
    n1' = Node { pos = pos n1, info = info n0 }

moveState :: Pos -> Pos -> NodesState -> NodesState
moveState c0 c1 nState = NodesState { nodes = ns' }
  where
    ns' = move c0 c1 $ nodes nState

nextStates :: NodesState -> [NodesState]
nextStates nState = map (\(c0, c1) -> moveState c0 c1 nState) moves
  where
    moves = viableMoves $ nodes nState

{- a more compact Show impl for debugging -}
instance Show NodesState where
  show NodesState { nodes = ns } =
      "{ns=" ++ nss ++ "}"
    where
      nss = foldl ((++) . (++ ";")) "" $ map nShow ns
      nShow n = "(" ++ show ((fst . pos) n)
          ++ "," ++ show ((snd . pos) n) ++ ","
          ++ show (info n) ++ ")"

-- Inform AStar this implements Eq, Hashable, Show
instance Searchable NodesState

{- find the optimal path to get data from desired node.
 - Astar is not sufficiently performant on large search spaces for this problem
 - by itself. It can be improved by doing 2 astar phases: one to get the Empty
 - node to the target, and another to get the target back to the start space.
 - -}
findPath :: [Node NodeKind] -> [NodesState]
findPath ns = path0 ++ tail path1
  where
    state0 = NodesState { nodes = ns }
    -- phase 0, goalDist is distance (Empty -> Data) - 1
    goalDist0 NodesState { nodes = ns } =
      distance (dataNode ns) (emptyNode ns) - 1
    -- phase 1, goalDist is distance of data to (0,0) [the user terminal]
    goalDist1 nState = let (x, y) = (pos . dataNode . nodes) nState in x + y
    path0 = astar nextStates goalDist0 state0
    path1 = trace "DEBUG: beginning path1" $ astar nextStates goalDist1 (last path0)
