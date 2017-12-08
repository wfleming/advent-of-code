module D7Lib  where

import Debug.Trace

import Data.List
import Data.Maybe
import Text.Megaparsec -- (ParseError, Dec, many)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Lexer as L

-- name & weight of each program
type Prog = (String, Int)

data ProgTree = Node
  { root ::Prog
  , children :: [ProgTree] } deriving (Eq, Show)

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

-- map children names to their full (name, weight) spec
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
stubTrees xs =
    map f xs
  where
    f (p, deps) = Node
      { root = p,
        children = map (\pd -> Node { root = pd, children = [] }) deps }

mvCandidates :: [ProgTree] -> ([ProgTree], [ProgTree])
mvCandidates ts =
    partition isCandidate ts
  where
    -- a node is a candidate if none of its children still appear in
    -- the top-level list, but it appears in a different node's children
    isCandidate n = noChildrenInList n && isAnotherNodesChild n
    noChildrenInList Node { children = cs } =
      null
      ( filter
        (\Node { root = r } -> r `elem` (map root cs))
        ts)
    isAnotherNodesChild Node { root = r } =
      not $ null
      ( filter
        (\Node { children = cs } -> r `elem` (map root cs))
        ts)

-- for tree (R, [...]), it looks for another tree with R in the forest, and
-- replaces that entry with the given one
mvTreeToSub :: ProgTree -> [ProgTree] -> [ProgTree]
mvTreeToSub x@Node { root = xr }  xs =
    trace ("mvTreeToSub " ++ (show xr) ++ " into " ++ (show (length xs)) ++ " trees") $
    case findIndex f xs of
      Nothing -> error $ "not expected: couldn't find " ++ (show xr) ++ " in " ++ (show xs)
      Just i -> mv i
  where
    f Node { children = deps } = xr `elem` (map root deps)
    replace x xs = map (\y -> if (root y) == (root x) then x else y) xs
    mv i = h ++ (xParent' : t)
      where
        (h, t) = splitAt i xs
        (xParent : t') = t
        xParent' = Node
          { root = root xParent
          , children = replace x (children xParent) }

collapseTree :: [ProgTree] -> ProgTree
collapseTree [t] = t
collapseTree ts =
    if (null toBeMoved)
      then (error ("nobody to move among " ++ (show (length ts)) ++ " trees"))
      else collapseTree $ foldl' mvTreeToSub' others toBeMoved
  where
    mvTreeToSub' xs x = mvTreeToSub x xs
    (toBeMoved, others) =
        trace ("calling mvCandidates with " ++ (show (length ts)) ++ " trees") $
        mvCandidates ts

buildTree :: [(Prog, [String])] -> ProgTree
buildTree = collapseTree . stubTrees . buildWeights
