module D13Lib  where

import Data.List
import Data.Maybe
import Data.Void
import Text.Megaparsec (parse, endBy, Parsec, ParseError)
import Text.Megaparsec.Char (string, eol)
import Text.Megaparsec.Char.Lexer as L

import Debug.Trace

type Position = Int
type Range = Int
type Depth = Int
type Severity = Int
type StepSize = Int
data Layer = Layer Depth Range Position StepSize deriving (Eq, Show)
data Firewall = Firewall [Layer] Position Severity

type Parser = Parsec Void String
type ParseResult = Either (ParseError Char Void) [Layer]

parseFile :: String -> IO ParseResult
parseFile path = do
  contents <- readFile path
  return $ parse layersParser path contents

layerParser :: Parser Layer
layerParser = do
  i <- L.decimal
  string ": "
  d <- L.decimal
  return $ Layer i d 0 1

layersParser :: Parser [Layer]
layersParser = layerParser `endBy` eol

stepLayer :: Layer -> Layer
stepLayer (Layer d r p s)
    | p + s < 0  = Layer d r 1 (0 - s)
    | p + s >= r = Layer d r (r - 2) (0 - s)
    | otherwise  = Layer d r (p + s) s

initFirewall :: [Layer] -> Firewall
initFirewall ls = Firewall ls (-1) 0

stepFirewall :: Firewall -> Firewall
stepFirewall (Firewall ls p s) = do
  -- 1. move the packet
  let p' = p + 1
  -- 2. incr severity if caught
  let s' = s + severityScore ls p'
  -- 3. incr all scanners
  let ls' = map stepLayer ls
  -- 4. done!
  Firewall ls' p' s'

runFirewall :: Firewall -> Firewall
runFirewall f
    | terminal f = f
    | otherwise = runFirewall $ stepFirewall f

terminal :: Firewall -> Bool
terminal f@(Firewall _ p _) = p >= upperRange f

upperRange :: Firewall -> Position
upperRange (Firewall ls _ _ ) =
    maximum depths
  where
    depths = map (\(Layer d _ _ _) -> d) ls

caught :: [Layer] -> Position -> Bool
caught [] _ = False
caught ((Layer lp _ 0 _):t) p | lp == p = True
caught (h:t) p = caught t p

severity :: Layer -> Severity
severity (Layer d r _ _) = d * r

accumulatedSeverity :: Firewall -> Severity
accumulatedSeverity (Firewall _ _ s) = s

severityScore :: [Layer] -> Position -> Severity
severityScore ls p =
    if caught ls p then layerSev
    else 0
  where
    layerSev = fromMaybe 0 $ severity <$> (find (\(Layer d _ _ _) -> d == p) ls)

-- find a delay that won't get caught at all
findDelay :: Firewall -> Position
findDelay f@(Firewall ls p s)
    | (not . getsCaught) f =
      -- trace ("findDelay finishing with p = " ++ (show p)) $
      abs $ p + 1
    | otherwise =
      -- trace ("findDelay stepping with p = " ++ (show p)) $
      findDelay (Firewall ls (p - 1) s)
  where
    getsCaught :: Firewall -> Bool
    getsCaught f@(Firewall ls p _)
        | terminal f = False
        | otherwise = do
            let p' = p + 1
            let ls' = map stepLayer ls
            if caught ls p' then True else getsCaught (Firewall ls' p' 0)

findDelay' :: Firewall -> Position
findDelay' f@(Firewall ls p _)
    | (not . getsCaught ls) p =
      -- trace ("findDelay finishing with p = " ++ (show p)) $
      abs $ p + 1
    | otherwise =
      -- trace ("findDelay stepping with p = " ++ (show p)) $
      findDelay' (Firewall ls (p - 1) 0)
  where
    getsCaught :: [Layer] -> Position -> Bool
    getsCaught ls p = any (lCaught p) ls
    -- If the packet stars at Position, does it get caught in Layer
    lCaught :: Position -> Layer -> Bool
    lCaught p l@(Layer d r _ _) =
        p' == 0
      where
        (Layer d r p' _) = layerAtT l $ d - p - 1

-- Calculate a layer's state at time t all at once
layerAtT :: Layer -> Int -> Layer
layerAtT (Layer d r _ _) t =
    Layer d r p' 0
  where
    p = t `mod` (2 * r - 2)
    p' = if p >= r then 2 * r - 2 - p else p
