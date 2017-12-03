module Main (main) where

import D9Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile decompressV1 file
  let msg = either (error . show) id parseResult

  putStrLn $ "decompressed message length: " ++ (show . length . stripAll) msg

  parseResult2 <- parseFromFile decompressV2 file
  let v2len = either (error . show) id parseResult2

  putStrLn $ "decompressed message length: " ++ show v2len
