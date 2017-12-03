module Main (main) where

import D2Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile parser file
  let keysSimple = either (error . show) (code 5) parseResult
  let keysWacky = either (error . show) (code' 5) parseResult
  putStrLn "simple keypad code is: "
  print keysSimple
  putStrLn "wacky keypad code is: "
  print keysWacky
