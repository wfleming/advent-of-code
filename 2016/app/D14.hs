module Main (main) where

import D14Lib

main :: IO ()
main = do
  let ks = findKeys "zpqevtbw"

  putStrLn $ "(P1) there are: " ++ show (length ks) ++ " keys found"
  putStrLn $ "(P1) last key index: " ++ show ((fst . last) ks)

  let ks2 = findKeys2 "zpqevtbw"

  putStrLn $ "(P2) there are: " ++ show (length ks2) ++ " keys found"
  putStrLn $ "(P2) last key index: " ++ show ((fst . last) ks2)
