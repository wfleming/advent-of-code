module D12Lib  where

import Data.Graph
import Data.List (foldl', nub, sort)
import Data.Void
import Text.Megaparsec (parse, endBy, sepBy, Parsec, ParseError, Token)
import Text.Megaparsec.Char (string, eol)
import Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
type ParseResult = Either (ParseError Char Void) [(Int, [Int])]

parseFile :: String -> IO ParseResult
parseFile path = do
  contents <- readFile path
  return $ parse parser path contents

parser :: Parser [(Int, [Int])]
parser = parseLine `endBy` eol

parseLine :: Parser (Int, [Int])
parseLine = do
  node <- L.decimal
  string " <-> "
  connected <- L.decimal `sepBy` string ", "
  return (node, connected)

buildBounds :: [(Int, [Int])] -> Bounds
buildBounds d =
    (minimum ns, maximum ns)
  where
    ns = map fst d

buildEdges :: [(Int, [Int])] -> [Edge]
buildEdges = concatMap (\(n, cs) -> zip (repeat n) cs)

buildGraph :: [(Int, [Int])] -> Graph
buildGraph d = buildG (buildBounds d) (buildEdges d)

group :: Graph -> Vertex -> [Vertex]
group g v = sort $ reachable g v

groups :: Graph -> [[Vertex]]
groups g = nub $ map (group g) (vertices g)
