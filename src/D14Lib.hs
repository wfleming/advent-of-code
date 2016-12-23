module D14Lib where

import Data.Maybe
import Data.List
import Util

{- | Find the first triplet char in a string, if one exists -}
rep3Char :: String -> Maybe Char
rep3Char (c1 : c2 : c3 : t)
    | c1 == c2 && c2 == c3 = Just c1
    | otherwise = rep3Char (c2 : c3 : t)
rep3Char _ = Nothing

{- | Determine if a given char repeats in a string 5 times consecutively -}
rep5Char :: Char -> String -> Bool
rep5Char target (c1 : c2 : c3 : c4 : c5 : t)
    | c1 == c2 && c2 == c3 && c3 == c4 && c4 == c5 && target == c1 = True
    | otherwise = rep5Char target (c2 : c3 : c4 : c5 : t)
rep5Char _ _ = False

buildHashes :: [Int] -> String -> [(Int, String)]
buildHashes idxs salt = map (\i -> (i, md5 $ salt ++ show i)) idxs

buildHashes2 :: [Int] -> String -> [(Int, String)]
buildHashes2 idxs salt = map (\i -> (i, stretch 2016 (salt ++ show i))) idxs
  where
    stretch 0 s = md5 s
    stretch c s = stretch (c - 1) $ md5 s

{- | Determines if hash at head of list is a valid key -}
isKey :: [(Int, String)] -> Bool
isKey ((_, h) : t) = isJust targetChar && any hasTarget5 (take 1000 t)
  where
    targetChar = rep3Char h
    hasTarget5 = rep5Char (fromMaybe (error "wtf") targetChar) . snd
isKey _ = False

keyCount :: Int
keyCount = 64

findKeys :: String -> [(Int, String)]
findKeys = keyFinder buildHashes

findKeys2 :: String -> [(Int, String)]
findKeys2 = keyFinder buildHashes2

-- It'd be nice if I could use the infinite list [0..], but something about
-- `foldtails` or `f` seems to be eager?
keyFinder :: ([Int] -> String -> [(Int, String)]) -> String -> [(Int, String)]
keyFinder hasher = take keyCount . foldtails f [] . hasher [0..99999]
  where
    f :: [(Int, String)] -> [(Int, String)] -> [(Int, String)]
    f keys keyCandidateL = if isKey keyCandidateL
        then keys ++ [head keyCandidateL]
        else keys


{- Like fold but it passes all the tails to the fn, not individual elements -}
foldtails :: (b -> [a] -> b) -> b -> [a] -> b
foldtails f memo xs@(h:t) = foldtails f (f memo xs) t
foldtails _ memo [] = memo
