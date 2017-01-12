module Main (main) where

import D22Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
    file <- head <$> getArgs
    parseResult <- parseFromFile nodesP file
    let nodes = either (error . show) id parseResult

    let p1ViableNodes = viablePairs nodes

    putStrLn $ "(P1) viable pair count: " ++ show (length p1ViableNodes)
