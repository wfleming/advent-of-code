module D6Lib where

import Data.List
import Util

fixMessage :: [String] -> String
fixMessage = fixer sorter
  where
    sorter (c1, f1) (c2, f2) = compare f2 f1 -- want freq-desc

fixModMessage :: [String] -> String
fixModMessage = fixer sorter
  where
    sorter (c1, f1) (c2, f2) = compare f1 f2 -- want freq-asc

fixer :: (Freq -> Freq -> Ordering) -> [String] -> String
fixer sorter = map (fst . head) . colFrequencies . transpose
  where
    colFrequencies = map $ sortBy sorter . freq
