module Main (main) where

import D19Lib

elves :: [Int]
elves = [1..3005290]

main :: IO ()
main = do
  let e1 = lastStanding elves
  putStrLn $ "(P1) last elf standing: " ++ show e1

  let e2 = lastStanding2 elves
  putStrLn $ "(P2) last elf standing: " ++ show e2
