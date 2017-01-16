{-# LANGUAGE DeriveGeneric #-}
module D22Lib where

import Debug.Trace

import Control.Monad
import Data.List (find)
import Data.Hashable (Hashable)
import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat)
import Data.Ord (comparing)
import GHC.Generics (Generic)
import Text.Parsec hiding (Error)
import Text.Parsec.String
import qualified PathSearch as PS

data Node = Node
    { pos :: (Int, Int)
    , capacity :: Int
    , used :: Int
    } deriving (Eq, Show, Generic)

data NodesState = NodesState
    { _nodes :: [Node]
    , curPos :: (Int, Int) -- where the target data currently is
    } deriving (Eq, Generic)

nodesP :: Parser [Node]
nodesP = do
  is <- nodeP `sepEndBy1` newline
  void $ eof
  return is

nodeP :: Parser Node
nodeP = do
    void $ string "/dev/grid/node-x"
    x <- numberP
    void $ string "-y"
    y <- numberP
    void $ many space
    capacity <- numberP
    void $ char 'T' >> many space
    used <- numberP
    void $ char 'T' >> manyTill anyChar (lookAhead newline)
    return $ Node { pos = (x, y), capacity = capacity, used = used }

numberP :: Parser Int
numberP = read <$> many1 digit

free :: Node -> Int
free n = capacity n - used n

viablePairs :: [Node] -> [(Node, Node)]
viablePairs = filter viable . pairs
  where
    pairs ns = concatMap (\n -> zip ns (repeat n)) ns
    viable (n1, n2) =
        pos n1 /= pos n2 &&
        used n1 > 0 &&
        used n1 <= free n2

sourceNode :: [Node] -> Node
sourceNode [n] = n
sourceNode (n0:n1:t) = case pos n0 of
  (x0, 0) -> let (x1, y1) = pos n1 in
      if 0 == y1 && x1 > x0
      then sourceNode (n1:t)
      else sourceNode (n0:t)
  _ -> sourceNode (n1:t)

initState :: [Node] -> NodesState
initState ns = NodesState { _nodes = ns, curPos = (pos . sourceNode) ns }

get :: (Int, Int) -> [Node] -> Node
get p = fromMaybe (error "no such coord") . find ((p==) . pos)

replace :: Node -> [Node] -> [Node]
replace _ [] = []
replace n (h:t) = if pos h == pos n
    then n:t
    else h : replace n t

move :: (Int, Int) -> (Int, Int) -> [Node] -> [Node]
move p0 p1 ns = replace n1' $ replace n0' ns
  where
    n0 = get p0 ns
    n1 = get p1 ns
    n0' = Node { pos = pos n0, capacity = capacity n0, used = 0 }
    n1' = Node { pos = pos n1, capacity = capacity n1, used = used n0 + used n1 }

viableMoves :: [Node] -> [((Int, Int), (Int, Int))]
viableMoves =
    filter neighbors . map (\(n1, n2) -> (pos n1, pos n2)) . viablePairs
  where
    neighbors ((x0, y0), (x1, y1)) =
        (x0 `nextTo` x1 && y0 == y1) || (y0 `nextTo` y1 && x0 == x1)
    nextTo n0 n1 = n0 + 1 == n1 || n0 - 1 == n1

moveState :: (Int, Int) -> (Int, Int) -> NodesState -> NodesState
moveState c0 c1 nState = NodesState { _nodes = ns', curPos = curPos' }
  where
    ns' = move c0 c1 $ _nodes nState
    curPos' = if curPos nState == c0 then c1 else curPos nState

instance PS.PathState NodesState where
    nextStates nState = map (\(c0, c1) -> moveState c0 c1 nState) moves
      where
        moves = viableMoves $ _nodes nState

    goalDist nState = let (x, y) = curPos nState in x + y


{- a more compact Show impl for debugging -}
instance Show NodesState where
  show NodesState { _nodes = ns, curPos = cp } =
      "{ns=" ++ nss ++ " cp=" ++ show cp ++ "}"
    where
      nss = foldl ((++) . (++ ";")) "" $ map nShow ns
      nShow n = show (used n)

instance Hashable Node -- default impl from Generic
instance Hashable NodesState -- default impl from Generic
