module D7Lib  where

import Data.List
import Data.Maybe
import Data.Tree
import Text.Megaparsec -- (ParseError, Dec, many)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Lexer as L

-- name & weight of each program
type Prog = (String, Int)

type ProgTree = Tree Prog

type ParseResult = Either (ParseError Char Dec) [(Prog, [String])]

parseInt :: Parser Int
parseInt = fromIntegral <$> L.integer :: Parser Int

progParser :: Parser Prog
progParser = do
  name <- many letterChar
  string " ("
  weight <- parseInt
  char ')'
  return (name, weight)

progDepParser :: Parser [String]
progDepParser = do
  string " -> "
  many letterChar `sepBy` string ", "

lineParser :: Parser (Prog, [String])
lineParser = do
  p <- progParser
  deps <- option [] progDepParser
  eol
  return (p, deps)

parser :: Parser [(Prog, [String])]
parser = many lineParser

-- determine root node name without actually building the tree
rootName :: [(Prog, [String])] -> String
rootName xs =
    fst . head . map fst . filter f $ xs
  where
    f ((n, _), _) = isNothing $ find (\(_, ns) -> n `elem` ns) xs

buildWeights :: [(Prog, [String])] -> [(Prog, [Prog])]
buildWeights xs =
    map f xs
  where
    fullNodes = map fst xs
    f (p, deps) = (p, map g deps)
    g depName = case find (\(n, _) -> n == depName) fullNodes of
      Nothing -> error "a dep with no weight given!"
      Just depProg -> depProg

stubTrees :: [(Prog, [Prog])] -> [ProgTree]
stubTrees =
    map f
  where
    f (p, deps) = Node
        { rootLabel = p
        , subForest = map (\x -> Node { rootLabel = x, subForest = [] }) deps
        }

-- for tree (R, [...]), it looks for another tree with R in the forest, and
-- replaces that entry with the given one
-- mvTreeToSub ProgTree -> [ProgTree]
-- mvTreeToSub x xs

-- buildTree :: [(Prog, [String])] -> ProgTree
-- buildTree [t] = t
buildTree ts =  undefined
