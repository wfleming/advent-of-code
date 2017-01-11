{-# LANGUAGE Strict #-}
module D19Lib where

import Debug.Trace

import Data.Maybe
import Util
import qualified Data.Sequence as S

lastStanding :: [Int] -> Int
lastStanding es = head $ until ((==1) . length) doRound es

doRound :: [Int] -> [Int]
doRound es = step [] es
  where
    step es' (e0:e1:t) = step (e0:es') t
    -- es' is being built backwards for perf, so we drop last el in this case...
    step es' [e0] = step (init (e0:es')) []
    -- and reverse es' before returning the final result
    step es' [] = reverse es'

lastStanding2 :: [Int] -> Int
{-lastStanding2 es = (flip S.index) 0 $ until ((==1) . S.length) doRound2 (S.fromList es)-}
lastStanding2 es = (flip S.index) 0 $ doRound2 (S.fromList es)

{-doRound2 :: S.Seq Int -> S.Seq Int
doRound2 elves = trace ("doRound2 elves.len=" ++ show (S.length elves)) $ step elves 0
  where
    step es i
        | i >= S.length es = es
        | otherwise =
            let
              es' = S.deleteAt (acrossIdx es i) es
              curVal = fromMaybe (error ("bad curVal at " ++ (show i) ++ " in " ++ show es)) $ (S.!?) es i
              i' = 1 + fromMaybe (error "bad i'") (S.elemIndexL curVal es')
            in
              debugTrace $ (step (seq' es') (seq' i'))
      where
        debugTrace = if (S.length es) `mod` 10000 == 0
            then trace ("step i=" ++ (show i) ++ " len=" ++ (show (S.length es)))
            else seq "noop"-}

{- See  https://github.com/glguy/advent2016/blob/master/Day19.hs
 - for a much more performant take: my commented-out impl above wastes lots of
 - time in `elemIndexL`. The key insight in glguy's is to avoid re-looking up
 - indexes by shifting current el to end of seq so the whole algorithm is one
 - long iteration. This is a quick take adapting that idea to what I already had
 - here.
 -}
doRound2 :: S.Seq a -> S.Seq a
doRound2 s
    | S.length s == 1 = s
    | otherwise = let
        (h, t) = S.splitAt 1 $ S.deleteAt (acrossIdx s 0) s
      in
        doRound2 (t S.>< h)


acrossIdx :: S.Seq a -> Int -> Int
acrossIdx v i = (i + (S.length v `div` 2)) `mod` S.length v

