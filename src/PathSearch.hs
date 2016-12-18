module PathSearch
    ( Path(..)
    , PathState(..)
    , PathSearch.length
    , minPath
    , isLoop
    , pp
    )
where

import Debug.Trace
import qualified Data.List as L

maxStates = 1000 -- to prevent giant search space growth

data Path a = Path { states :: [a] } deriving (Eq, Show)

length :: Path a -> Int
length = L.length . states

instance Eq a => Ord (Path a) where
  compare p1 p2 = compare (PathSearch.length p1) (PathSearch.length p2)

class (Eq t, Show t) => PathState t where
    {- | construct a list of valid next states for the given path -}
    nextStates :: t -> [t]

    {- | return true if the current state is a goal state -}
    isGoal :: t -> Bool

    {- | calculate how far a state is from a goal state -}
    goalDist :: t -> Int

{- | Find the shortest path from an initial state to a goal state -}
minPath :: PathState a => a -> Path a
minPath s = process [Path [s]]
  where
    process = L.head . L.sort . filter isGoalPath . until goalFound buildNextPaths
    isGoalPath = isGoal . L.last . states
    goalFound = L.any isGoalPath

buildNextPaths :: PathState a => [Path a] -> [Path a]
buildNextPaths l | L.null l = error "buildNextPaths empty list: won't ever find a solution."
buildNextPaths l | L.length l > maxStates =
    trace ("There are now " ++ (show . L.length) l ++ " states in the search space: trimming. Current leader is " ++ (show . goalDist . L.last . states . L.head) trimmedStates ++ " from goal.")
    $ buildNextPaths trimmedStates
  where
    trimmedStates = ((take maxStates . L.sortBy sorter) l)
    sorter p1 p2 = compare (extractDist p1) (extractDist p2)
    extractDist = goalDist . L.last . states
buildNextPaths l =
    {-trace ("----- DEBUG -----\nCOUNT=" ++ show (L.length l) ++ "\n" ++-}
      {-pp l ++-}
      {-"\n loopStates=" ++ show (L.filter isLoop candidatePaths) ++-}
      {-"\n dupStates=" ++ show (L.filter (hasDupCurState candidatePaths) candidatePaths) ++-}
      {-"\n----------------\n")-}
    trace ("----- DEBUG -----\nCOUNT=" ++ show (L.length l))
    $ L.foldl' dropUnlessOk candidatePaths candidatePaths
  where
    candidatePaths = L.concatMap buildNextPath l
    dropUnlessOk memo p = if isOk memo p then memo else L.delete p memo
    isOk ps p = not (isLoop p || hasDupCurState ps p)

-- for debug output
pp :: PathState a => [Path a] -> String
pp l = "[\n" ++ (unlines . L.intersperse "-----------------" ) (map (("\t" ++) . show . last . states) l) ++ "\n]"

{- | Construct all the valid next paths from a given path.
     Paths which have no valid next states return `[]`.
-}
buildNextPath :: PathState a => Path a -> [Path a]
buildNextPath Path { states = ss } = if L.null newStates
    then []
    else map (\s -> Path (ss ++ [s])) newStates
  where
    newStates = nextStates $ L.last ss

-- Determines if a path involves a loop back to a previous state
isLoop :: PathState a => Path a -> Bool
isLoop Path { states = ss } = any ((>1) . delCount) ss
  where
    delCount ps = (L.length ss) - (L.length (L.filter (/=ps) ss))

-- Determines if a path's last state is in any other current path
hasDupCurState :: PathState a => [Path a] -> Path a -> Bool
hasDupCurState ps p = L.any ((s `L.elem`) . states)  otherPs
  where
    otherPs = L.delete p ps
    s = L.last $ states p
