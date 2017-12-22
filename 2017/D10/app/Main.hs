module Main where

import D10Lib
import System.Environment (getArgs)

import qualified Data.Digest.Knot as K

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile parser file
  let lengths = either (error . show) id parseResult
  let s0 = K.new [0..255] lengths
  let (K.State nsFinal _ _ _) = K.runRound s0
  putStrLn $ "p1: first 2 ns = " ++ (show (take 2 nsFinal))
  putStrLn $ "p1: product = " ++ (show ((product . take 2) nsFinal))
  inputStr <- readFile file
  let h = K.hash inputStr
  putStrLn $ "p2 hashed = " ++ h
