module D4Lib where

import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String

data Room = Room
    { name :: String
    , sector :: Int
    , checksum :: String
    } deriving (Eq, Show)
type Freq = (Char, Int)

checksumLength = 5

-- room is valid if the checksum is the 5 most common letters in the name
valid :: Room -> Bool
valid (Room name _ checksum) = checksum == calcChecksum name

calcChecksum :: String -> String
calcChecksum = map fst . take checksumLength . sortBy compFreq . freq
  where
    compFreq :: Freq -> Freq -> Ordering
    compFreq (c1, f1) (c2, f2) = case f1 == f2 of
        True -> compare c1 c2 -- ties should be sorted alphabetically
        False -> compare f2 f1 -- non-ties are sorted freq-desc

freq :: String -> [Freq]
freq = map charfreq . group . sort . filter (/= '-')
  where
    charfreq l | h : _ <- l = (h, length l)
    charfreq _ = error "empty freq list?"

decryptRoom :: Room -> Room
decryptRoom (Room n s c) = Room (decryptName n s) s c

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
