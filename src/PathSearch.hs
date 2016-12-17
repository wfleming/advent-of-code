module PathSearch
    ( Path(..)
    , PathState(..)
    , minPath
    )
where

import qualified Data.List as L

data Path a = Path { states :: [a] } deriving (Eq, Show)

length :: Path a -> Int
length = L.length . states

instance Eq a => Ord (Path a) where
  compare p1 p2 = compare (PathSearch.length p1) (PathSearch.length p2)

class Eq t => PathState t where
    {- | construct a list of valid next states for the given path -}
    nextStates :: t -> [t]

    {- | return true if the current state is a goal state -}
    isGoal :: t -> Bool

{- | Find the shortest path from an initial state to a goal state -}
minPath :: PathState a => a -> Path a
minPath s = process [Path [s]]
  where
    process = L.head . L.sort . filter isGoalPath . until goalFound buildNextPaths
    isGoalPath = isGoal . L.last . states
    goalFound = L.any isGoalPath

buildNextPaths :: PathState a => [Path a] -> [Path a]
buildNextPaths = L.concatMap buildNextPath

{- | Construct all the valid next paths from a given path.
     Paths which have no valid next states return `[]`.
-}
buildNextPath :: PathState a =>  Path a -> [Path a]
buildNextPath Path { states = ss } = if L.null newStates
    then []
    else map (\s -> Path (ss ++ [s])) newStates
  where
    newStates = nextStates $ L.last ss
