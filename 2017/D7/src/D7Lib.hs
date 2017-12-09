module D7Lib  where

import Data.List
import Data.Maybe
import Data.Tree
import Text.Megaparsec
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

-- determine rootLabel node name without actually building the tree
rootName :: [(Prog, [String])] -> String
rootName xs =
    fst . head . map fst . filter f $ xs
  where
    f ((n, _), _) = isNothing $ find (\(_, ns) -> n `elem` ns) xs

-- map subForest names to their full (name, weight) spec
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
      { rootLabel = p,
        subForest = map (\pd -> Node { rootLabel = pd, subForest = [] }) deps }

mvCandidates :: [ProgTree] -> ([ProgTree], [ProgTree])
mvCandidates ts =
    partition isCandidate ts
  where
    -- a node is a candidate if none of its subForest still appear in
    -- the top-level list, but it appears in a different node's children
    isCandidate n = noChildrenInList n && isAnotherNodesChild n
    noChildrenInList Node { subForest = cs } =
      null
      ( filter
        (\Node { rootLabel = r } -> r `elem` (map rootLabel cs))
        ts)
    isAnotherNodesChild Node { rootLabel = r } =
      not $ null
      ( filter
        (\Node { subForest = cs } -> r `elem` (map rootLabel cs))
        ts)

-- for tree (R, [...]), it looks for another tree with R in the forest, and
-- replaces that entry with the given one
mvTreeToSub :: ProgTree -> [ProgTree] -> [ProgTree]
mvTreeToSub x@Node { rootLabel = xr }  xs =
    case findIndex f xs of
      Nothing -> error $ "not expected: couldn't find " ++ (show xr) ++ " in " ++ (show xs)
      Just i -> mv i
  where
    f Node { subForest = deps } = xr `elem` (map rootLabel deps)
    replace x xs = map (\y -> if (rootLabel y) == (rootLabel x) then x else y) xs
    mv i = h ++ (xParent' : t')
      where
        (h, t) = splitAt i xs
        (xParent : t') = t
        xParent' = Node
          { rootLabel = rootLabel xParent
          , subForest = replace x (subForest xParent) }

collapseTree :: [ProgTree] -> ProgTree
collapseTree [t] = t
collapseTree ts =
    if (null toBeMoved)
      then (error ("nobody to move among " ++ (show (length ts)) ++ " trees"))
      else collapseTree $ foldl' mvTreeToSub' others toBeMoved
  where
    mvTreeToSub' xs x = mvTreeToSub x xs
    (toBeMoved, others) = mvCandidates ts

buildTree :: [(Prog, [String])] -> ProgTree
buildTree = collapseTree . stubTrees . buildWeights

treeWeight :: ProgTree -> Int
treeWeight Node { rootLabel = (_, w), subForest = cs }
  =  (+ w) . sum . map treeWeight $ cs

uniq :: Eq a => [a] -> [a]
uniq = reverse . foldl (\xs x -> if x `elem` xs then xs else (x : xs)) []

balanced :: ProgTree -> Bool
balanced Node { subForest = cs } =
    (length (uniq ws)) == 1
  where
    ws = map treeWeight cs

unbalanced :: ProgTree -> Bool
unbalanced = not . balanced

unbalancedLayer :: ProgTree -> Maybe [ProgTree]
unbalancedLayer t@Node { subForest = cs }
  | (length . uniq . map treeWeight) cs > 1 = Just cs
  | otherwise = subForest <$> find (isJust . unbalancedLayer) cs
  -- TODO: need to descend until the children aren't unbalanced

-- do DFS into children to find the lowest unbalanced tree
unbalancedCulprit :: ProgTree -> Maybe ProgTree
unbalancedCulprit t@Node { subForest = cs }
  | balanced t = Nothing
  | all balanced cs = Just t
  | otherwise = (find unbalanced cs) >>= unbalancedCulprit

-- find the  siblings of a prog in the tree
-- siblings :: Prog -> ProgTree -> [ProgTree]
-- siblings needle Node { subForest = cs }
--   | null cs = cs
--   | (isJust . find (\t -> rootLabel t == needle)) cs = cs
--   | otherwise = concatMap (siblings needle) cs

drawableTree :: ProgTree -> Tree String
drawableTree t@Node { rootLabel = (n, w), subForest = cs }
  = Node
    { rootLabel = "(" ++ n ++ ", " ++ (show w) ++ ", " ++ (show (treeWeight t)) ++ ")"
    , subForest = map drawableTree cs
    }
