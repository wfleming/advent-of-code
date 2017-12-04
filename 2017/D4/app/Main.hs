module Main where

import D4Lib
import Text.Megaparsec
import Text.Megaparsec.String
import System.Environment (getArgs)

parseFile :: Parser [Passphrase] -> String -> IO ParseResult
parseFile p path = do
  contents <- readFile path
  return $ parse p path contents

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile parser file
  let passphrases = either (error . show) id parseResult
  putStrLn $ "p1: number of valid passphrases: " ++ (show (validCount passphrases))
  putStrLn $ "p2: number of valid anagram passphrases: " ++ (show (validAnagramCount passphrases))
