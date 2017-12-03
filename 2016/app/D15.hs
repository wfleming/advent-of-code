module Main (main) where

import D15Lib

main :: IO ()
main = do
  let t0 =
        [ Disc { count = 17, pos = 15 }
        , Disc { count = 3, pos = 2 }
        , Disc { count = 19, pos = 4 }
        , Disc { count = 13, pos = 2 }
        , Disc { count = 7, pos = 2 }
        , Disc { count = 5, pos = 0 }
        ]
  let pressAt = firstPass t0

  putStrLn $ "(P1) press the button at t=" ++ show (pressAt)

  let t0' = t0 ++ [ Disc { count = 11, pos = 0 } ]
  let pressAt' = firstPass t0'

  putStrLn $ "(P2) press the button at t=" ++ show (pressAt')
