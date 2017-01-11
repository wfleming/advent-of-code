module Main (main) where

import D20Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile blockListP file
  let blockList = either (error . show) id parseResult

  let p1Lowest = lowestAllowed blockList
  putStrLn $ "(P1) lowest allowed IP: " ++ show (p1Lowest)

  let p2count = allowedCount blockList
  putStrLn $ "(P2) total number allowed: " ++ show (p2count)
