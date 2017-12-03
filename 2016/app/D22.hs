module Main (main) where

import D22Lib
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
    file <- head <$> getArgs
    parseResult <- parseFromFile nodesP file
    let initialNodes = either (error . show) id parseResult

    let p1ViableNodes = viablePairs initialNodes

    putStrLn $ "(P1) viable pair count: " ++ show (length p1ViableNodes)

    let p2Path = findPath $ simplify initialNodes

    putStrLn $ "(P2) shortest path to goal: " ++ show (length p2Path - 1)
