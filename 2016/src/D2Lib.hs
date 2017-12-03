module D2Lib where

import Data.List
import Data.Maybe
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String

type Key = Int -- 1 through 9 (or through 13 for the weird keypad)
data Move = U | D | L | R deriving (Eq, Show)

move :: Key -> Move -> Key
move k U = if k - 3 > 0 then k - 3 else k
move k D = if k + 3 <= 9 then k + 3 else k
move k L = max rowMin (k - 1)
  where
    rowMin = 1 + 3 * floor ((fromIntegral k :: Float) / 3.1)
move k R = min rowMax (k + 1)
  where
    rowMax = 3 + 3 * floor ((fromIntegral k :: Float) / 3.1)

parser :: Parser [[Move]]
parser = many parseMove `sepBy` char '\n'

-- There's really gotta be a cleaner way of defining a tokenizing parser
parseMove :: Parser Move
parseMove = choice
    [  p' 'U' U
    ,  p' 'D' D
    ,  p' 'L' L
    ,  p' 'R' R
    ]
  where
    p' :: Char -> Move -> Parser Move
    p' c t = do
        _ <- char c
        return t

key :: Key -> [Move] -> Key
key = foldl move

-- tail to drop the inital start value in the array, and init because the input
-- file has an extra newline resulting in a final [] set of moves
code :: Key -> [[Move]] -> [Key]
code start moveArrs = init $ tail $ scanl key start moveArrs

keypad =
    [ [Nothing, Nothing, Just 1, Nothing, Nothing]
    , [Nothing, Just 2, Just 3, Just 4, Nothing]
    , [Just 5, Just 6, Just 7, Just 8, Just 9]
    , [Nothing, Just 10, Just 11, Just 12, Nothing]
    , [Nothing, Nothing, Just 13, Nothing, Nothing]
    ]

-- movement on the weird second keypad
move' :: Key -> Move -> Key
move' start move = fromMaybe start next
  where
    row :: [Maybe Key]
    row = fromMaybe (error "couldn't find row") $ find (elem $ Just start) keypad
    rowIndex = fromMaybe (error "Couldn't find row idx") $ elemIndex row keypad
    colIndex = fromMaybe (error "couldn't find key in row") $ elemIndex (Just start) row
    rowIndex' = case move of
        U -> max 0 (rowIndex - 1)
        D -> min 4 (rowIndex + 1)
        _ -> rowIndex
    colIndex' = case move of
        L -> max 0 (colIndex - 1)
        R -> min 4 (colIndex + 1)
        _ -> colIndex
    next :: Maybe Key
    next = keypad !! rowIndex' !! colIndex'

-- get the code on the weird second keypad
code' :: Key -> [[Move]] -> [Key]
code' start moveArrs = init $ tail $ scanl key' start moveArrs
  where
    key' = foldl move'

