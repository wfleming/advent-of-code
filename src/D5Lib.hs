{-# LANGUAGE ScopedTypeVariables #-}
module D5Lib where

import Crypto.Hash
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC

type CharPos = (Char, Int)

passwordLength = 8
maxStretches = 100000000 -- reasonable limit to detect infinite recursion
stretches = [0..]

password :: String -> String
password seed = take passwordLength $ passPieces $ passChar seed

-- Not sure what I did, but this impl is non-terminating, or just VERY poor
-- performing
{-orderedPassword :: String -> String-}
{-orderedPassword seed = process $ passPieces $ orderedPassChar seed-}
  {-where-}
    {-process = map fst . sortBy sorter . take passwordLength . foldl reduce []-}
    {-sorter (_, i1) (_, i2) = compare i1 i2-}
    {-reduce :: [CharPos] -> CharPos -> [CharPos]-}
    {-reduce xs (c, i) = if i `elem` (map snd xs)-}
       {-then xs-}
       {-else (c, i) : xs-}

orderedPassword :: String -> String
orderedPassword seed = map fst . sortBy sorter $ build [] 0
  where
    sorter (_, i1) (_, i2) = compare i1 i2
    build :: [CharPos] -> Int -> [CharPos]
    build xs _ | length xs >= passwordLength = xs
    build xs i | i > maxStretches = error $ "Too many iterations xs=" ++ show xs ++ " i=" ++ show i
    build xs i = case orderedPassChar seed i of
        Just (c, ci) -> if ci `elem` map snd xs
            then build xs (i + 1)
            else build ((c, ci) : xs) (i + 1)
        Nothing -> build xs (i + 1)

passPieces :: forall a. (Int -> Maybe a) -> [a]
passPieces f = process stretches
  where
    process = map (fromMaybe err) . filter isJust . map f
    err = error "wtf"

passChar :: String -> Int -> Maybe Char
passChar seed idx = (!! 5) <$> idxHash seed idx

orderedPassChar :: String -> Int -> Maybe CharPos
orderedPassChar seed idx = idxHash seed idx >>= extract
  where
    extract str = if validPos str
        then Just (str !! 6, rInt [str !! 5])
        else Nothing
    rInt = read :: String -> Int
    validPos str = isDigit (str !! 5) && rInt [str !! 5] < 8

idxHash :: String -> Int -> Maybe String
idxHash _ idx | idx > maxStretches = error $ "Too many iterations (might be infinite) idx=" ++ show idx
idxHash seed idx =
    if "00000" `isPrefixOf` hash
        then Just hash
        else Nothing
  where
    hash = (LC.unpack . LC.fromStrict . digestToHexByteString . md5) inputBs
    inputBs = LC.pack (seed ++ show idx)
    md5 :: LB.ByteString -> Digest MD5
    md5 = hashlazy


