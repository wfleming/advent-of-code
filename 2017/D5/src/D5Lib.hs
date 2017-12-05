module D5Lib  where

import Text.Megaparsec (ParseError, Dec, endBy)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Char as C
import Text.Megaparsec.Lexer as L
import Data.List (splitAt)


type ParseResult = Either (ParseError Char Dec) Jmps

type Jmps = [Int]

data State = State
    { pos :: Int
    , jmps :: Jmps
    } deriving (Show, Eq)

parser :: Parser Jmps
parser = (L.signed C.space pInt) `endBy` eol
  where
    pInt = fromIntegral <$> L.integer

start :: Jmps -> State
start js = State { pos = 0, jmps = js }

-- returns Nothing if pos is outside list
step :: (Int -> Int) -> State -> Maybe State
step _ State { pos = p } | p < 0 = Nothing
step _ State { pos = p, jmps = js } | p >= length js = Nothing
step jmpIncr State { pos = p, jmps = js } =
    Just $ State { pos = p', jmps = js' }
  where
    (h, (curJmp : t)) = splitAt p js
    js' = h ++ ((jmpIncr curJmp) : t)
    p' = p + curJmp

stepP1 = step (+ 1)
stepP2 =
    step f
  where
    f x = if x >= 3 then x - 1 else x + 1

-- starting from a given state, run until a terminal state
run :: (State -> Maybe State) -> State -> [State]
run stepper s0 =
    s0 : rest
  where
    rest = case stepper s0 of
      Nothing -> []
      Just s -> run stepper s

-- starting from a given state, run until a terminal state
-- don't track intermediate states, just count the number of steps taken
run' :: (State -> Maybe State) -> State -> Int
run' stepper s0 =
    1 + rest
  where
    rest = case stepper s0 of
      Nothing -> 0
      Just s -> run' stepper s
