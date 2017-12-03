module Main (main) where

import D8Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile cardParser file
  let card = either (error . show) id parseResult
  let scr = screen 6 50
  let scr' = runCard card scr

  print scr'
  putStrLn $ "\nThere are " ++ show (litPixels scr') ++ " lit pixels"

