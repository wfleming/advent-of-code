module Util where

import Data.List

type Freq = (Char, Int)

-- determine the frequency of each char in a string
freq :: String -> [Freq]
freq = map charfreq . group . sort . filter (/= '-')
  where
    charfreq l | h : _ <- l = (h, length l)
    charfreq _ = error "empty freq list?"
