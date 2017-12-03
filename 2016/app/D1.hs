module Main (main) where

import System.Environment (getArgs)
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String
import Text.Printf

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile parseMovements file
  let movements = either (error . show) id parseResult
  let finalPos = walk (0,0) movements
  putStrLn $ printf "Walked to " ++ show finalPos ++ " total distance = " ++ show (dist (0,0) finalPos)
  let dupPos = hitTwice ((0, 0), North) (expand movements) []
  putStrLn $ printf "first loc seen twice is " ++ show dupPos ++ " total distance = " ++ show (dist (0,0) dupPos)

type Point = (Int, Int)
type Distance = Int
data Turn = TurnRight | TurnLeft | Straight
type Movement = (Turn, Distance)
data Orientation = North | South | West | East

walk :: Point -> [Movement] -> Point
walk start moves = fst $ foldl step (start, North) moves

hitTwice :: (Point, Orientation) -> [Movement] -> [Point] -> Point
hitTwice (start, _) [] placesBeen = error $ "exhausted moves without seeing any point twice placesBeen=" ++ show (start : placesBeen)
hitTwice (start, facing) (nextMove : restMoves) placesBeen =
    if start `elem` placesBeen
        then start
        else hitTwice (step (start, facing) nextMove) restMoves (start : placesBeen)

expand :: [Movement] -> [Movement]
expand = concatMap expandMove
  where
    expandMove (t, n) | n >= 1 = (t, 1) : replicate (n - 1) (Straight, 1)
    expandMove _ = error "accidental negative maybe?"

step :: (Point, Orientation) -> Movement -> (Point, Orientation)
step (pos, facing) (moveTurn, moveDist) = (newPos, newDir)
  where
    newDir = turn facing moveTurn
    newPos = performMove pos newDir moveDist

    turn North TurnRight = East
    turn North TurnLeft = West
    turn South TurnRight = West
    turn South TurnLeft = East
    turn West TurnRight = North
    turn West TurnLeft = South
    turn East TurnRight = South
    turn East TurnLeft = North
    turn dir Straight = dir

    performMove (x, y) North d = (x, y + d)
    performMove (x, y) South d = performMove (x, y) North (negate d)
    performMove (x, y) East d = (x + d, y)
    performMove (x, y) West d = performMove (x, y) East (negate d)

dist :: Point -> Point -> Int
dist (x1, y1) (x2, y2) = abs(x2 - x1) + abs(y2 - y1)

-- Input is a number of movements separated by spaces
parseMovements :: Parser [Movement]
parseMovements = parseMovement `sepBy` string ", "

-- A movement is a direction & an integral distance
parseMovement :: Parser Movement
parseMovement = choice
    [ movementParser 'R' (\d -> (TurnRight, d))
    , movementParser 'L' (\d -> (TurnLeft, d))
    ]

movementParser :: Char -> (Int -> Movement) -> Parser Movement
movementParser dirToken ctor = do
    _ <- char dirToken
    return ctor <*> parseDistance

parseDistance :: Parser Int
parseDistance = read <$> many1 digit
