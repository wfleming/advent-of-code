module Main (main) where

import D18Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile rowP file
  let row = either (error . show) id parseResult

  putStrLn $ "(P1) number of safe tiles: " ++ show (nsafe row 40)
  putStrLn $ "(P2) number of safe tiles: " ++ show (nsafe row 400000)
