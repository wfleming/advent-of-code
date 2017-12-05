module Main where

import D5Lib
import Text.Megaparsec
import Text.Megaparsec.String
import System.Environment (getArgs)

parseFile :: Parser Jmps -> String -> IO ParseResult
parseFile p path = do
  contents <- readFile path
  return $ parse p path contents

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile parser file
  let stepCount1 = (run' stepP1) . start <$> parseResult
  putStrLn $ "p1: steps taken = " ++ (show stepCount1)
  let stepCount2 = (run' stepP2) . start <$> parseResult
  putStrLn $ "p2: steps taken = " ++ (show stepCount2)
