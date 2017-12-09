module Main where

import D7Lib
import Data.Tree
-- import Data.Maybe
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
  let rootNodeName = rootName <$> parseResult
  putStrLn $ "p1: root node is " ++ (show rootNodeName)
  let tree = buildTree <$> parseResult
  let culprit = unbalancedCulprit <$> tree
  let culpritStr = either show (maybe "Nothing" (drawTree . drawableTree)) culprit
  putStrLn $ "p2: the smallest unbalanced tree is \n" ++ culpritStr
  let tree' = either (error . show) id tree
  let layer = unbalancedLayer tree'
  let layerStr = maybe ["no unbalanced layer found"] (map (drawTree . drawableTree)) layer
  let layerLines = unlines layerStr
  putStrLn $ "p2: the entire unbalanced layer is\n" ++ layerLines
  -- let culprit' = either (error "no culprit found") fromJust culprit
  -- let sibs = siblings (rootLabel culprit') tree'
  -- putStrLn $ "p2: the weight of its siblings is " ++ (show (map treeWeight sibs))
