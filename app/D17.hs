module Main (main) where

import D17Lib
import System.Environment (getArgs)
import qualified PathSearch as PS

main :: IO ()
main = do
  vaultSeed <- head <$> getArgs
  let vault = start vaultSeed
  let finalVault = last . PS.states . PS.minPath $ vault
  putStrLn $ "shortest path with seed '" ++ vaultSeed ++ "':  " ++ path finalVault

  let finalVault' = maxPath vault
  putStrLn $ "longest path with seed '" ++ vaultSeed ++ "':  " ++ (show . length . path) finalVault'

