module Main where

import D18Lib
import D18Lib.Parser as P
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  opsM <- parseFile file
  let m = newMachine <$> opsM
  let m' = runUntil willRecover <$> m
  putStrLn $ "p1: last sounds played = " ++ (show (mLastPlayed <$> m'))
