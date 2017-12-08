module Main where

import D7Lib
import Text.Megaparsec
import Text.Megaparsec.String
import System.Environment (getArgs)

parseFile :: Parser [(Prog, [String])] -> String -> IO ParseResult
parseFile p path = do
  contents <- readFile path
  return $ parse p path contents

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile parser file
  let root = rootName <$> parseResult
  putStrLn $ "p1: root node is " ++ (show root)
