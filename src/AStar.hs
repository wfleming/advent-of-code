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

type StepMap a = HashMap a (Maybe a)

{- debug: allow keeping trace statements without too much noise in tests -}
{-trace :: a -> b -> b-}
{-trace _ b = b-}

{- | Find an efficient path using A* -}
astar :: (Hashable a, PathState a) => a -> [a]
astar start = step (Seq.viewl (Seq.singleton start)) Seq.empty (HashMap.singleton start Nothing)

{- algorithm loop:
 - * Candidates must be sorted by fScore when passed in
 - * seen is the set of candidates already processed
 - * cameFrom maps node -> best previous node -}
step :: (Hashable a, PathState a) => Seq.ViewL a -> Seq a -> StepMap a -> [a]
step Seq.EmptyL _ _ = error "empty candidate set: possibly no solution exists"
step (c Seq.:< cs) seen cameFrom
    | isGoal c = backtrack cameFrom c
    | otherwise =
        trace (
            "astar.step c.goaldist=" ++ show (goalDist c) ++
            " last.goaldist=" ++ show lastGoalDist ++
            " cs.length=" ++ (show (Seq.length cs)) ++
            " seen.length=" ++ (show (Seq.length seen)) ++
            " cameFrom.size=" ++ (show (HashMap.size cameFrom))
            ) $
        step (Seq.viewl cs') (seen Seq.|> c) cameFrom'
      where
        neighbors = nextStates c
        cameFrom' = insertIfBetter cameFrom c neighbors
        cs' = Seq.unstableSortBy
            (comparing (fScore cameFrom')) $
            seqAppendL cs (filter (not . (memberEither cs seen)) neighbors)
        lastGoalDist = if Seq.length cs > 0 then (goalDist $ sLast cs) else -1

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

sLast :: Seq a -> a
sLast s = let (_ Seq.:> e) = Seq.viewr s in e

notMember :: (Eq a, Hashable a) => a -> HashMap a b -> Bool
notMember k m = not $ HashMap.member k m

memberEither :: Eq a => Seq a -> Seq a -> a -> Bool
memberEither s1 s2 e = e `elem` s1 || e `elem` s2

seqAppendL :: Seq a -> [a] -> Seq a
seqAppendL = foldl' (Seq.|>)

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
