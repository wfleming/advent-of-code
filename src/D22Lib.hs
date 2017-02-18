{-# LANGUAGE DeriveGeneric #-}
module D22Lib (
  Node(..),
  NodeKind(..),
  NodesState(..),
  Pos,
  capacity,
  findPath,
  get,
  initState,
  move,
  nodesP,
  simplify,
  sourceNode,
  used,
  viableMoves,
  viablePairs
) where

{-import Astar-}
import D22Lib.Node
import D22Lib.Parser
import Data.List (find)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
{-import Data.Monoid (mconcat)-}
import Data.Ord (comparing)
import GHC.Generics (Generic)
import qualified PathSearch as PS

data NodesState = NodesState
    { nodes :: [Node NodeKind]
    , curPos :: Pos -- where the target data currently is
    } deriving (Eq, Generic)

instance Hashable NodesState

-- The node at (x = max x, y = 0)
sourceNode :: [Node a] -> (Node a)
sourceNode [n] = n
sourceNode (n0:n1:t) = case pos n0 of
  (x0, 0) -> let (x1, y1) = pos n1 in
      if 0 == y1 && x1 > x0
      then sourceNode (n1:t)
      else sourceNode (n0:t)
  _ -> sourceNode (n1:t)

initState :: [Node NodeKind] -> NodesState
initState ns = NodesState { nodes = ns, curPos = (pos . sourceNode) ns }

get :: Pos -> [Node a] -> Node a
get p = fromMaybe (error "no such coord") . find ((p==) . pos)

replace :: (Node a) -> [Node a] -> [Node a]
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
moveState c0 c1 nState = NodesState { nodes = ns', curPos = curPos' }
  where
    ns' = move c0 c1 $ nodes nState
    curPos' = if curPos nState == c0 then c1 else curPos nState

instance PS.PathState NodesState where
    nextStates nState = map (\(c0, c1) -> moveState c0 c1 nState) moves
      where
        moves = viableMoves $ nodes nState

    goalDist nState = let (x, y) = curPos nState in x + y

{- a more compact Show impl for debugging -}
instance Show NodesState where
  show NodesState { nodes = ns, curPos = cp } =
      "{ns=" ++ nss ++ " cp=" ++ show cp ++ "}"
    where
      nss = foldl ((++) . (++ ";")) "" $ map nShow ns
      nShow n = show (info n)

{- find the optimal path to get data from desired node -}
findPath :: [Node NodeKind] -> [a]
findPath = error "unimplemented"

