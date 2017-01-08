{-# LANGUAGE Strict #-}
module D16Lib where

import Util

curve :: Int -> String -> String
curve len = seq' $ take len . until ((>=len) . length) curveStep

curveStep :: String -> String
curveStep a = seq' $ a ++ "0" ++ b
  where
    b = (map swap . reverse) a
    swap c = case c of
        '0' -> '1'
        '1' -> '0'
        _ -> c

checksum :: String -> String
checksum input
    | null input = error "can't calc checksum of empty str"
    | odd (length input) = error "can't calc checksum of odd length str"
    | otherwise = until (odd . length) cs input
  where
    cs str = seq' (reverse $ cs' "" str)
    cs' memo (c0:c1:t) = cs' (seq' nextMemo) t
      where
        nextChar = if c0 == c1 then '1' else '0'
        nextMemo = nextChar : memo
    cs' memo [] = memo
