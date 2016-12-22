module Main (main) where

import D12Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile instructionsP file
  let instructions = either (error . show) id parseResult
  let vm = newVM instructions
  let vmRan = run vm

  putStrLn $ "final VM part 1: " ++ show vmRan

  let vm2 = newVM2 instructions
  let vm2Ran = run vm2
  putStrLn $ "final VM part 2: " ++ show vm2Ran
