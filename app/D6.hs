module Main (main) where

import D6Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  fileLines <- lines <$> readFile file
  let msg = fixMessage fileLines

  putStrLn $ "message: " ++ msg

  let msg' = fixModMessage fileLines

  putStrLn $ "message using modified rep code: " ++ msg'

