{-# LANGUAGE Strict #-}
module Main (main) where

import D16Lib

main :: IO ()
main = do
  let seed = "11100010111110100"
  let d = curve 272 seed
  let csum = checksum d

  putStrLn $ "(P1) checksum=" ++ csum

  putStrLn $ "(P2) starting to generate random data"
  let d' = curve 35651584 seed
  putStrLn $ "(P2) starting to calculate checksum"
  let csum' = checksum d'

  putStrLn $ "(P2) checksum=" ++ csum'
