module Main (main) where

import D5Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  passSeed <- head <$> getArgs
  let pass = password passSeed
  putStrLn $ "by order encountered: " ++ passSeed ++ " -> " ++ pass

  let ordPass = orderedPassword passSeed
  putStrLn $ "by val at index 6: " ++ passSeed ++ " -> " ++ ordPass

