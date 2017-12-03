{-# LANGUAGE Strict #-}
module D18Lib where

import Data.Maybe
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String
import qualified Data.Vector as V

data Tile = Trap | Safe deriving (Eq, Show)

type Row = V.Vector Tile

rowP :: Parser Row
rowP = do
    tiles <- many1 tileP
    return $ V.fromList tiles

tileP :: Parser Tile
tileP = do
    c <- char '.' <|> char '^'
    return $ if c == '.' then Safe else Trap

rownsafe :: Row -> Int
rownsafe = V.length . V.filter (==Safe)

nsafe :: Row -> Int -> Int
nsafe row n = iter row 0 n
  where
    iter r memo 0 = memo
    iter r memo n' = iter (succRow r) (memo + rownsafe r) (n' - 1)

succRow :: Row -> Row
succRow v = V.imap (\i _ -> safe v i) v

safe :: Row -> Int -> Tile
safe prev pos = case influencers prev pos of
    (Trap, Trap, Safe) -> Trap
    (Safe, Trap, Trap) -> Trap
    (Safe, Safe, Trap) -> Trap
    (Trap, Safe, Safe) -> Trap
    _ -> Safe

influencers :: Row -> Int -> (Tile, Tile, Tile)
influencers v i = (get (i - 1), get i, get (i + 1))
  where
    get i' = fromMaybe Safe $ (V.!?) v i'
