module Main (main) where

import D21Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

p1input :: String
p1input = "abcdefgh"

p2input :: String
p2input = "fbgdceah"

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile instructionsP file
  let instructions = either (error . show) id parseResult

  let p1scrambled = scramble p1input instructions

  putStrLn $ "(P1) scrambled: " ++ p1scrambled

  let p2unscrambled = unscramble p2input instructions

  putStrLn $ "(P2) unscrambled: " ++ p2unscrambled
