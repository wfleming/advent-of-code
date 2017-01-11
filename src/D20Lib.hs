{-# LANGUAGE Strict #-}
module D20Lib where

import Control.Monad
import Data.Ord (comparing)
import Data.List (foldl', sort)
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String
import Util

ipIntMin = 0
ipIntMax = 4294967295

data BlockRule = BlockRule { low :: Int, high :: Int } deriving (Eq, Show)

instance Ord BlockRule where
    compare = comparing low

blockListP :: Parser [BlockRule]
blockListP = blockRuleP `sepEndBy1` newline

blockRuleP :: Parser BlockRule
blockRuleP = do
  l <- numberP
  void $ char '-'
  h <- numberP
  return $ BlockRule l h

numberP :: Parser Int
numberP = read <$> many1 digit

blocked :: [BlockRule] -> Int -> Bool
blocked rs ip = any (\r -> ip >= (low r) && ip <= (high r)) rs

nubBlockList :: [BlockRule] -> [BlockRule]
nubBlockList = step . sort
  where
    step (r1:r2:t) = if combinable r1 r2
                     then step $ (combine r1 r2) : t
                     else r1 : (step (r2 : t))
    step rs = rs

-- r1 must sort lower than r2
combinable :: BlockRule -> BlockRule -> Bool
combinable r1 r2 = overlap || adjacent
  where
    overlap = (low r1) <= (low r2) && (high r1) >= (low r2)
    adjacent = (high r1) == (low r2 - 1)

combine :: BlockRule -> BlockRule -> BlockRule
combine r1 r2 = BlockRule l h
  where
    l = min (low r1) (low r2)
    h = max (high r1) (high r2)

lowestAllowed :: [BlockRule] -> Int
lowestAllowed rs = step 0
  where
    step i | i < ipIntMin || i > ipIntMax = error ("invalid int IP=" ++ show i)
           | blocked rs i = step (i + 1)
           | otherwise = i

allowedCount :: [BlockRule] -> Int
allowedCount = foldl' step (ipIntMax + 1) . nubBlockList
  where
    step c r = c - (1 + high r - low r)
