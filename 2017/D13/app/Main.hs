module Main where

import D13Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile file
  let f0 = initFirewall <$> parseResult
  let fFinal = runFirewall <$> f0
  let s = accumulatedSeverity <$> fFinal
  putStrLn $ "p1: severity accumulated = " ++ (show s)
  let d = findDelay' <$> f0
  putStrLn $ "p2: minimum delay to not get caught = " ++ (show d)
