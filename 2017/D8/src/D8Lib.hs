module D8Lib  where

import qualified Data.Map as M
import Text.Megaparsec (parse)
import Text.Megaparsec.String (Parser)
import D8Lib.Data
import qualified D8Lib.Parser as P

parseFile :: Parser [Rule] -> String -> IO P.ParseResult
parseFile p path = do
  contents <- readFile path
  return $ parse p path contents

largestRegVal :: Registers -> Int
largestRegVal = maximum . M.elems
