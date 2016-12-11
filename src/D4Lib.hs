{-# ANN letters "HLint: ignore Use String" #-}
module D4Lib where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Util
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String

data Room = Room
    { name :: String
    , sector :: Int
    , checksum :: String
    } deriving (Eq, Show)

checksumLength = 5

-- room is valid if the checksum is the 5 most common letters in the name
valid :: Room -> Bool
valid (Room name _ checksum) = checksum == calcChecksum name

calcChecksum :: String -> String
calcChecksum = map fst . take checksumLength . sortBy compFreq . freq
  where
    compFreq :: Freq -> Freq -> Ordering
    compFreq (c1, f1) (c2, f2) = if f1 == f2
        then compare c1 c2 -- ties should be sorted alphabetically
        else compare f2 f1 -- non-ties are sorted freq-desc

decryptRoom :: Room -> Room
decryptRoom (Room n s c) = Room (decryptName n s) s c

-- This is conceptually a list of chars, so I'm ignoring HLint
letters :: [Char]
letters = ['a'..'z']

decryptName :: String -> Int -> String
decryptName str shift = map tr str
  where
    tr c = case c of
        '-' -> ' '
        c | c `elem` letters -> infLetters !! (cIdx c + shift)
        _ -> error "not a dash or lowercase letter"
    infLetters = cycle letters
    cIdx c = fromMaybe (error "tried to look up non-letter") $ elemIndex c letters

parser :: Parser [Room]
parser = many parseRoom

parseRoom :: Parser Room
parseRoom = do
    n <- many (lower <|> char '-')
    s <- number
    void $ char '['
    c <- count checksumLength lower
    void $ char ']'
    newline
    return Room { name = n, sector = s, checksum = c }

number :: Parser Int
number = read <$> many1 digit
