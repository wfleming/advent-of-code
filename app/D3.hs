module Main (main) where

import D3Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile parser file
  let triangles = either (error . show) id parseResult
  let countValidByRow = length $ filter valid triangles
  let countValidByCol = length $ filter valid $ colTriangles triangles
  putStrLn "number of valid triangles (by row): "
  print countValidByRow
  putStrLn "number of valid triangles (by col): "
  print countValidByCol
