module Main where

import D10Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile parser file
  let lengths = either (error . show) id parseResult
  let s0 = new [0..255] lengths
  let (State nsFinal _ _ _) = runRound s0
  putStrLn $ "p1: first 2 ns = " ++ (show (take 2 nsFinal))
  putStrLn $ "p1: product = " ++ (show ((product . take 2) nsFinal))
  inputStr <- readFile file
  let h = hash inputStr
  putStrLn $ "p2 hashed = " ++ h
