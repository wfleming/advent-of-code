module D15Lib where

data Disc = Disc { count :: Int, pos :: Int } deriving (Eq, Show)

{- | Advance a disc by t = 1 -}
tick :: Disc -> Disc
tick = tickBy 1

tickBy :: Int -> Disc -> Disc
tickBy n Disc { count = c, pos = p } =
    Disc { count = c, pos = (p + n) `mod` c }

{- | Determines if the capsule passes a set of discs -}
pass :: [Disc] -> Bool
pass (h:t) = if 0 == pos (tick h)
    then pass (map tick t)
    else False
pass [] = True

{- | Find the earliest t at which a capsule will pass -}
firstPass :: [Disc] -> Int
firstPass ds = fst $ until (pass . snd) tickPair (0, ds)
  where
    tickPair (t, ds') = (t + 1, map tick ds')
