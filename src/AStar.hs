module AStar (astar) where

import Debug.Trace

import Data.List (foldl', nub)
import Data.HashMap.Strict (HashMap)
import Data.Hashable (Hashable)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Sequence (Seq)
import PathSearch (PathState(..))
import Util
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set as Set
import qualified Data.Sequence as Seq

type SeenMap a = HashMap a Bool
type StepMap a = HashMap a (Maybe a)

{- perf improvement opportunities:
 - store gscore with nodes somewhere to avoid calcing up again with `backtrack`
   (actually not sure if Haskell is smart enough to memoize internally: if so,
   this won't help)
 -}

{-debug: allow keeping trace statements without too much noise in tests -}
{-trace :: a -> b -> b-}
{-trace _ b = b-}

{- | Find an efficient path using A* -}
astar :: (Hashable a, PathState a) => a -> [a]
astar start = step
    (Seq.viewl (Seq.singleton start))
    HashMap.empty
    (HashMap.singleton start Nothing)

{- algorithm loop:
 - * Candidates must be sorted by fScore when passed in
 - * seen is the set of candidates already processed
 - * cameFrom maps node -> best previous node -}
step :: (Hashable a, PathState a) => Seq.ViewL a -> SeenMap a -> StepMap a -> [a]
step Seq.EmptyL _ _ = error "empty candidate set: possibly no solution exists"
step (c Seq.:< cs) seen cameFrom
    | isGoal c = backtrack cameFrom c
    | otherwise =
        trace (
            "astar.step c.goaldist=" ++ show (goalDist c) ++
            " c.fScore=" ++ show (fScore cameFrom c) ++
            " cs.length=" ++ (show (Seq.length cs)) ++
            " seen.size=" ++ (show (HashMap.size seen)) ++
            " cameFrom.size=" ++ (show (HashMap.size cameFrom))
            {-++ " state=\n" ++ debugState ++ "\n\n\n"-}
            ) $
        step (Seq.viewl cs') seen' cameFrom'
      where
        neighbors = nextStates c
        cameFrom' = insertIfBetter cameFrom c neighbors
        seen' = HashMap.insert c True seen
        cs' = foldl'
            (insertSorted (comparing (fScore cameFrom')))
            cs
            (filter (not . ((flip HashMap.member) seen)) neighbors)
        debugState = foldl'
            (\s c -> s ++ " # " ++ show c ++ "  fScore=" ++ show (fScore cameFrom c) ++ "\n")
            "" (c Seq.<| cs)

{- fScore is the expected total cost of moving from start -> goal through a
 - given node -}
fScore :: (Eq a, Hashable a, PathState a) => StepMap a -> a -> Int
fScore cameFrom n = goalDist n + gScore cameFrom n

{- gScore is the cost of getting to a node from the start node, e.g. the length
 - of the path to get there. -}
gScore :: (Eq a, Show a, Hashable a) => StepMap a -> a -> Int
gScore cameFrom = length . backtrack cameFrom

{- Construct a path from a node back to the start -}
backtrack :: (Eq a, Show a, Hashable a) => StepMap a -> a -> [a]
backtrack cameFrom = (seq' . reverse . btrack [])
  where
    btrack ns n
        | isCycle ns = error $ "cycle detected backtracking: cameFrom=" ++ (show cameFrom)
        | otherwise = case (HashMap.lookup n cameFrom) of
            Nothing -> error "looked for non-existent key in map"
            Just Nothing -> n:ns
            Just (Just n') -> btrack (n:ns) n'

notMember :: (Eq a, Hashable a) => a -> HashMap a b -> Bool
notMember k m = not $ HashMap.member k m

seqAppendL :: Seq a -> [a] -> Seq a
seqAppendL = foldl' (Seq.|>)

insertSorted :: Eq a => (a -> a -> Ordering) -> Seq a -> a -> Seq a
insertSorted f s e = case Seq.viewl s of
    Seq.EmptyL -> Seq.singleton e
    (h Seq.:< t) -> case f h e of
      LT -> h Seq.<| (insertSorted f t e)
      EQ -> if h == e
          then h Seq.<| t -- don't insert duplicates
          else h Seq.<| (insertSorted f t e)
      GT -> e Seq.<| (h Seq.<| t)

isCycle :: Eq a => [a] -> Bool
isCycle as = (length . nub) as /= length as

insertIfBetter :: (Hashable a, PathState a) => StepMap a -> a -> [a] -> StepMap a
insertIfBetter cameFrom from newCs = foldl' step cameFrom newCs
  where
    step map c'
      | c' == from = error "a node cannot be reached from itself"
      | notMember c' map = HashMap.insert c' (Just from) map
      | otherwise =
        -- gScore if added is gScore of from + 1
        if gScore map c' >= gScore map from
        then HashMap.insert c' (Just from) map -- new entry is better: replace
        else map -- old entry is better, don't change
