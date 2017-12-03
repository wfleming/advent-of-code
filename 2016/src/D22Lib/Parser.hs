module D22Lib.Parser where

import Control.Monad
import D22Lib.Node
import Text.Parsec hiding (Error)
import Text.Parsec.String

nodesP :: Parser [Node NodeSizes]
nodesP = do
  is <- nodeP `sepEndBy1` newline
  void $ eof
  return is

nodeP :: Parser (Node NodeSizes)
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
    return $ Node { pos = (x, y), info = (capacity, used) }

numberP :: Parser Int
numberP = read <$> many1 digit
