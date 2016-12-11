module D8Lib where

import Control.Monad
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String
import qualified Data.Matrix as M
import qualified Data.Vector as V


{------ SCREEN ----}

data Pixel = On | Off deriving (Eq)

instance Show Pixel where
    show On = "#"
    show Off = "."

newtype Screen = S { s :: M.Matrix Pixel } deriving (Eq)

instance Show Screen where
    show = unlines . mapRows showRow
      where
        showRow = concatMap show

screen :: Int -> Int -> Screen
screen rows cols = S $ M.matrix rows cols (const Off)

-- Matrices are 1 indexed?!
mapRows :: (V.Vector Pixel -> b) -> Screen -> [b]
mapRows f (S m) = map (f . flip M.getRow m) [1..(M.nrows m)]

setPixel :: Screen -> Pixel -> (Int, Int) -> Screen
setPixel (S m) v p = S $ M.setElem v p m

-- coordinates are (row, col)
getPixel :: Screen -> (Int, Int) -> Pixel
getPixel (S m) p = m M.! p

litPixels :: Screen -> Int
litPixels = length . filter id . concat . mapRows (map isOn . V.toList)
  where
    isOn On = True
    isOn Off = False

-- turn on pixels in upper left corner
rect :: Screen -> Int -> Int -> Screen
rect s rows cols = foldl (`setPixel` On) s points
  where
    rowPoints row = foldl (\ps i -> (row, i):ps) [] [1..cols]
    points = concatMap rowPoints [1..rows]

-- shift a row of the screen by an amount
rotRow :: Screen -> Int -> Int -> Screen
rotRow (S m) row shift = S $ M.mapRow setPix row m
  where
    sourceCol c = if c - shift < 1
        then M.ncols m - abs (c - shift)
        else c - shift
    setPix col _ = m M.! (row, sourceCol col)

-- shift a col of the screen by an amount
rotCol :: Screen -> Int -> Int -> Screen
rotCol (S m) col shift = S $ M.transpose m'
  where
    (S m') = rotRow (S $ M.transpose m) col shift


{------ CARD ----}

type Card = [CardInstruction]

data CardInstruction =
    Rect Int Int | RotRow Int Int | RotCol Int Int
    deriving (Show, Eq)

cardParser :: Parser Card
cardParser = many cardInstParser

cardInstParser :: Parser CardInstruction
cardInstParser  = do
  inst <- choice [try rectParser, try rotRowParser, try rotColParser]
  newline
  return inst

rectParser :: Parser CardInstruction
rectParser = do
  void $ string "rect "
  rows <- number
  void $ char 'x'
  cols <- number
  return $ Rect rows cols

rotRowParser :: Parser CardInstruction
rotRowParser = do
  void $ string "rotate row y="
  row <- number
  void $ string " by "
  shift <- number
  -- card are encoded 0-indexed, but our Screen is 1-indexed
  return $ RotRow (row + 1) shift

rotColParser :: Parser CardInstruction
rotColParser = do
  void $ string "rotate column x="
  col <- number
  void $ string " by "
  shift <- number
  -- card are encoded 0-indexed, but our Screen is 1-indexed
  return $ RotCol (col + 1) shift

number :: Parser Int
number = read <$> many1 digit

runCard :: Card -> Screen -> Screen
runCard c s = foldl runInst s c
  where
    runInst s' (Rect width height) = rect s' height width
    runInst s' (RotRow r s) = rotRow s' r s
    runInst s' (RotCol c s) = rotCol s' c s
