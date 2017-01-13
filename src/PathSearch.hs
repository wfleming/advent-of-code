module PathSearch
    ( Path(..)
    , PathState(..)
    , PathSearch.length
    , buildNextPaths
    , isLoop
    , minPath
    , pp
    , seqLast
    , PathSearch.last
    , singleton
    )
where

{-import Debug.Trace-}
import Data.Foldable (toList)
import Data.Ord (comparing)
import Data.Sequence (Seq)
import Util
import qualified Data.List as L
import qualified Data.Sequence as Seq

{- debug: allow keeping trace statements without too much noise in tests -}
trace :: a -> b -> b
trace _ b = b

maxStates = 10000 -- to prevent giant search space growth

data Path a = Path { states :: Seq a } deriving (Eq, Show)

length :: Path a -> Int
length = Seq.length . states

singleton :: a -> Path a
singleton s = Path (Seq.singleton s)

last :: Path a -> a
last = seqLast . states

seqLast :: Seq a -> a
seqLast s = s `Seq.index` (Seq.length s - 1)

seqDelete :: Eq a => a -> Seq a -> Seq a
seqDelete el s = case idx of
    Nothing -> s
    Just i -> Seq.deleteAt i s
  where
    idx = el `Seq.elemIndexL` s

seqConcatMap :: (a -> Seq b) -> Seq a -> Seq b
seqConcatMap f = foldr (\a bs -> bs Seq.>< f a) Seq.empty

seqHead :: Seq a -> a
seqHead = (`Seq.index` 0)

instance Eq a => Ord (Path a) where
    compare = comparing PathSearch.length

class (Eq t, Show t) => PathState t where
    {- | construct a list of valid next states for the given path -}
    nextStates :: t -> [t]

    {- | return true if the current state is a goal state -}
    isGoal :: t -> Bool
    isGoal = (== 0) . goalDist

    {- | calculate how far a state is from a goal state -}
    goalDist :: t -> Int

{- | Find the shortest path from an initial state to a goal state via BFS -}
minPath :: PathState a => a -> Path a
minPath s = process $ Seq.singleton (singleton s)
  where
    process = seqHead . Seq.sort . Seq.filter isGoalPath . until goalFound buildNextPaths
    isGoalPath = isGoal . seqLast . states
    goalFound = any isGoalPath

buildNextPaths :: PathState a => Seq (Path a) -> Seq (Path a)
buildNextPaths l | Seq.null l = error "buildNextPaths empty list: won't ever find a solution."
buildNextPaths l | Seq.length l > maxStates =
    trace ("There are now " ++ (show . Seq.length) l ++ " states in the search space: trimming. Current leader is " ++ (show . goalDist . seqLast . states . seqHead) trimmedStates ++ " from goal.")
    $ buildNextPaths trimmedStates
  where
    trimmedStates = ((Seq.take maxStates . Seq.sortBy sorter) l)
    sorter p1 p2 = compare (extractDist p1) (extractDist p2)
    extractDist = goalDist . seqLast . states
buildNextPaths l =
    {-trace ("----- DEBUG -----\nCOUNT=" ++ show (Seq.length l) ++ "\n" ++-}
      {-pp l ++-}
      {-"\n loopStates=" ++ show (Seq.filter isLoop candidatePaths) ++-}
      {-"\n dupStates=" ++ show (Seq.filter (hasDupCurState candidatePaths) candidatePaths) ++-}
      {-"\n----------------\n")-}
    trace ("----- DEBUG -----\nCOUNT=" ++ show (Seq.length l))
    $ L.foldl' dropUnlessOk candidatePaths candidatePaths
  where
    candidatePaths = seqConcatMap buildNextPath l
    dropUnlessOk memo p = if isOk memo p then memo else seqDelete p memo
    isOk ps p = not (isLoop p || hasDupCurState ps p)

-- for debug output
pp :: PathState a => Seq (Path a) -> String
pp l = "[\n" ++ (unlines . L.intersperse "-----------------" ) (map (("\t" ++) . show . L.last . toList . states) (toList l)) ++ "\n]"

{- | Construct all the valid next paths from a given path.
     Paths which have no valid next states return `Seq.null`.
-}
buildNextPath :: PathState a => Path a -> Seq (Path a)
buildNextPath Path { states = ss } = if Seq.null newStates
    then Seq.empty
    else Seq.mapWithIndex (\_ s -> Path (ss Seq.|> s)) newStates
  where
    newStates = Seq.fromList . nextStates $ seqLast ss

-- Determines if a path involves a loop back to a previous state
isLoop :: PathState a => Path a -> Bool
isLoop Path { states = ss } = any ((>1) . delCount) ss
  where
    delCount ps = (Seq.length ss) - (Seq.length (Seq.filter (/=ps) ss))

-- Determines if a path's last state is in any other current path
hasDupCurState :: PathState a => Seq (Path a) -> Path a -> Bool
hasDupCurState ps p = any ((s `elem`) . states)  otherPs
  where
    otherPs = seqDelete p ps
    s = seqLast $ states p
