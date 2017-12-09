module D8Lib  where

import Data.List
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

largestEver :: Registers -> [Rule] -> Int
largestEver regs =
    snd . foldl' f (regs, largestRegVal regs)
  where
    f (state, memo) r = (s', memo')
      where
        s' = apply state r
        memo' = if memo >= largestRegVal s' then memo else largestRegVal s'
