module D22Lib where

import Control.Monad
{-import Data.List (elemIndex, find, foldl', permutations, splitAt)-}
{-import Data.Maybe (fromMaybe)-}
import Text.Parsec hiding (Error)
import Text.Parsec.String

data Node = Node
    { pos :: (Int, Int)
    , capacity :: Int
    , used :: Int
    } deriving (Eq, Show)

nodesP :: Parser [Node]
nodesP = do
  is <- nodeP `sepEndBy1` newline
  void $ eof
  return is

nodeP :: Parser Node
nodeP = do
    void $ string "/dev/grid/node-x"
    x <- numberP
    void $ string "-y"
    y <- numberP
    void $ many space
    capacity <- numberP
    void $ char 'T' >> many space
    used <- numberP
    void $ char 'T' >> manyTill anyChar (lookAhead newline)
    return $ Node { pos = (x, y), capacity = capacity, used = used }

numberP :: Parser Int
numberP = read <$> many1 digit

free :: Node -> Int
free n = capacity n - used n

viablePairs :: [Node] -> [(Node, Node)]
viablePairs = filter viable . pairs
  where
    pairs ns = concatMap (\n -> zip ns (repeat n)) ns
    viable (n1, n2) =
        pos n1 /= pos n2 &&
        used n1 > 0 &&
        used n1 <= free n2
