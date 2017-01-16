{-# LANGUAGE DeriveGeneric #-}
module D11Lib where

import Data.Hashable (Hashable)
import Data.List
import GHC.Generics (Generic)
import qualified PathSearch as PS

data Element =
    Promethium | Cobalt | Curium | Plutonium | Ruthenium | Elerium | Dilithium
    deriving (Eq, Generic, Show)

data ItemType = Microchip | Generator deriving (Eq, Generic, Show)

type Item = (ItemType, Element)

data Floor = Floor { items :: [Item] } deriving (Generic, Show)

data State = State { floors :: [Floor], curFloor :: Int } deriving (Eq, Generic, Show)

initState = State
    { floors =
        [ Floor { items = [(Generator, Promethium), (Microchip, Promethium)] }
        , Floor { items = [(Generator, Cobalt), (Generator, Curium), (Generator, Ruthenium), (Generator, Plutonium)] }
        , Floor { items = [(Microchip, Cobalt), (Microchip, Curium), (Microchip, Ruthenium), (Microchip, Plutonium)] }
        , Floor { items = [] }
        ]
    ,  curFloor = 0
    }

initState2 = State
    { floors =
        -- FIXME: do I need to treat these as "isolated" correctly to get a valid
        -- result?
        [ Floor { items = [(Generator, Elerium), (Microchip, Elerium), (Generator, Dilithium), (Microchip, Dilithium), (Generator, Promethium), (Microchip, Promethium)] }
        , Floor { items = [(Generator, Cobalt), (Generator, Curium), (Generator, Ruthenium), (Generator, Plutonium)] }
        , Floor { items = [(Microchip, Cobalt), (Microchip, Curium), (Microchip, Ruthenium), (Microchip, Plutonium)] }
        , Floor { items = [] }
        ]
    ,  curFloor = 0
    }

-- default Hashable impls
instance Hashable Element
instance Hashable ItemType
instance Hashable Floor
instance Hashable State

instance Eq Floor where
  (==) f1 f2 = null $ (nub . items) f1 \\ (nub . items) f2

instance PS.PathState State where
    -- up and/or down, all valid changes from each
    nextStates s@State { curFloor = cf } | cf == 0 =
      filter valid $ moveStates s 1
    nextStates s@State { curFloor = cf } | cf == maxFloor s =
      filter valid $ moveStates s (-1)
    nextStates s =
      filter valid $ ((moveStates s (-1)) ++ (moveStates s 1))

    isGoal s = (curFloor s) == maxFloor s && lowerFloorsEmpty s
      where
        lowerFloorsEmpty = all (null . items) . take (maxFloor s) . floors

    goalDist s@State { floors = fs, curFloor = cf} =
        sum $ (maxFloor s - cf) : map floorDist (zip [0..] fs)
      where
        floorDist (idx, f) = (length . items) f * (maxFloor s - idx)

maxFloor :: State -> Int
maxFloor s = (length . floors) s - 1

isMicrochip :: Item -> Bool
isMicrochip (Microchip, _) = True
isMicrochip _ = False

isGenerator :: Item -> Bool
isGenerator (Generator, _) = True
isGenerator _ = False

valid :: State -> Bool
valid s = validFloors && validCurrentFloor
  where
    validFloors = (all validFloor . floors) s
    validCurrentFloor = curFloor s >= 0 && curFloor s <= maxFloor s

validFloor :: Floor -> Bool
validFloor Floor { items = its } =
    not (any isMicrochip unprotectedItems && any isGenerator its)
  where
    isChipProtected (Microchip, el) = (Generator, el) `elem` its
    isChipProtected _ = False
    unprotectedItems = filter (not . isChipProtected) its

-- Given a floor, construct a list of lists of items that could be moved
moveOpts :: Floor -> [[Item]]
moveOpts Floor { items = its } = filter allowed $ subsequences its
  where
    allowed l = (length l) == 1 || (length l) == 2

moveStates :: State -> Int -> [State]
moveStates s dir = map (applyMove s dir) (moveOpts (floors s !! curFloor s))

applyMove :: State -> Int -> [Item] -> State
applyMove State { floors = fs, curFloor = cf } dir moveItems = State
    { floors = replaceAt (replaceAt fs cf curFloor') (cf + dir) nextFloor'
    , curFloor = cf + dir
    }
  where
    curFloor' = Floor { items = (items (fs !! cf)) \\ moveItems }
    nextFloor' = Floor { items = (items (fs !! (cf + dir))) ++ moveItems }

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs i x = h ++ (x : (tail t))
  where
    (h, t) = splitAt i xs
