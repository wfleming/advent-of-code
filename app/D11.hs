module Main (main) where

import D11Lib
import qualified PathSearch as PS

main :: IO ()
main = do
  {-let p = PS.minPath initState-}
  let p = PS.minPath initState2
  {-let p = PS.minPath initStateEg-}

  putStrLn $ "path length: " ++ show (PS.length p) ++
      " (that's " ++ show (PS.length p - 1) ++ " steps.)"


{-initStateEg :: State-}
{-initStateEg = State-}
    {-{ floors =-}
        {-[ Floor { items = [(Microchip, Cobalt), (Microchip, Promethium)] }-}
        {-, Floor { items = [(Generator, Cobalt)] }-}
        {-, Floor { items = [(Generator, Promethium)] }-}
        {-, Floor { items = [] }-}
        {-]-}
    {-,  curFloor = 0-}
    {-}-}
