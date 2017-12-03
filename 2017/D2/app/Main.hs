module Main where

import D2Lib
import Text.Megaparsec
import Text.Megaparsec.String
import System.Environment (getArgs)

parseFile :: Parser Sheet -> String -> IO ParseResult
parseFile parser path = do
  contents <- readFile path
  return $ parse parser path contents

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile sheetParser file
  let sheet = either (error . show) id parseResult
  let csum1 = checksum1 sheet
  putStrLn $ "p1 checksum:" ++ (show csum1)
  let csum2 = checksum2 sheet
  putStrLn $ "p2 checksum:" ++ (show csum2)
