{-# LANGUAGE Strict #-}
module D17Lib where

import Data.Foldable (toList)
import Data.List
import Data.Maybe
import Data.Ord
import Data.Sequence (Seq)
import Util
import qualified Data.Sequence as Seq
import qualified PathSearch as PS

data Direction = U | D | L | R deriving (Eq, Show)

class (Show a) => Layout a where
    {- return which directions from a pos can be moved to -}
    validMoves :: a -> [Direction]

data Vault = Vault
    { pos :: (Int, Int) -- (x, y)
    , height :: Int
    , width :: Int
    , seed :: String
    , path :: String
    } deriving (Eq, Show)

instance Layout Vault where
    validMoves v@Vault { pos = (x, y), height = h, width = w } =
        filter (canMove v) [U, D, L, R]

instance PS.PathState Vault where
    nextStates v = (map (move v) . validMoves) v

    isGoal Vault { pos = (x, y), height = h, width = w }
        = x == w - 1 && y == h - 1

    goalDist Vault { pos = (x, y), height = h, width = w } = (w - x) + (h - y)

instance Ord Vault where
    compare = comparing (length . path)

start :: String -> Vault
start s = Vault
    { pos = (0, 0)
    , height = 4
    , width = 4
    , seed = s
    , path = ""
    }

canMove :: Vault -> Direction -> Bool
canMove v d = hasDoor v d && unlocked v d

hasDoor :: Vault -> Direction -> Bool
hasDoor Vault { pos = (x, y), height = h, width = w } d
    | d == U = y > 0
    | d == D = y < h - 1
    | d == L = x > 0
    | d == R = x < w - 1

unlocked :: Vault -> Direction -> Bool
unlocked Vault { seed = s, path = p } d = doorChar d `elem` openChars
  where
    hash = md5 (s ++ p)
    openChars = ['b'..'f']
    doorChar U = hash !! 0
    doorChar D = hash !! 1
    doorChar L = hash !! 2
    doorChar R = hash !! 3

move :: Vault -> Direction -> Vault
move v@Vault { pos = (x, y) } d
    | canMove v d = Vault
        { pos = apply d
        , height = height v
        , width = width v
        , seed = seed v
        , path = path v ++ show d }
    | otherwise = error "can't move in that direction"
  where
    apply U = (x, y - 1)
    apply D = (x, y + 1)
    apply L = (x - 1, y)
    apply R = (x + 1, y)

{- find the *maximum* path that works for a goal. Unlike `PS.minPath`, this is
 - designed specifically for `Vault` (which tracks history internally),
 - so this doesn't bother with `Path` to track history for the return val -}
maxPath :: Vault -> Vault
maxPath s = step Nothing [PS.Path (Seq.singleton s)]
  where
    step :: Maybe Vault -> [PS.Path Vault] -> Vault
    step Nothing [] = error "No states left, no goals found"
    step (Just g) [] = g
    step g states = step (seq' g') (seq' states')
      where
        nextVaults = map
            (PS.last) $
            (toList . PS.buildNextPaths . Seq.fromList) states
        goals = filter PS.isGoal nextVaults
        bestGoal = listToMaybe . reverse . sortOn (length . path) $ goals
        g' = last $ sort [g, bestGoal]
        states' = map (PS.singleton) (nextVaults \\ goals)
