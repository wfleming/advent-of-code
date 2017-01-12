module D21Lib where

import Control.Monad
import Data.List (elemIndex, find, foldl', permutations, splitAt)
import Data.Maybe (fromMaybe)
import Text.Parsec hiding (Error)
import Text.Parsec.String

data Dir = L | R deriving (Eq, Show)

data Instruction =
    SwapPos Int Int |
    SwapLet Char Char |
    RotatePos Char |
    RotateDir Dir Int |
    Move Int Int |
    Reverse Int Int
    deriving (Eq, Show)

instructionsP :: Parser [Instruction]
instructionsP = do
  is <- instructionP `sepEndBy1` newline
  void $ eof
  return is

instructionP :: Parser Instruction
instructionP = choice $ map try [swapPosP, swapLetP, rotP, movP, revP]

swapPosP :: Parser Instruction
swapPosP = do
    void $ string "swap position "
    op1 <- numberP
    void $ string " with position "
    op2 <- numberP
    return $ SwapPos op1 op2

swapLetP :: Parser Instruction
swapLetP = do
    void $ string "swap letter "
    op1 <- lower
    void $ string " with letter "
    op2 <- lower
    return $ SwapLet op1 op2

rotP :: Parser Instruction
rotP = (try rotDirP) <|> (try rotPosP)

rotPosP :: Parser Instruction
rotPosP = do
    void $ string "rotate based on position of letter "
    op1 <- lower
    return $ RotatePos op1

rotDirP :: Parser Instruction
rotDirP = do
    void $ string "rotate "
    op1 <- string "left" <|> string "right"
    void $ space
    op2 <- numberP
    void $ space
    void $ string "step"
    void $ optional $ char 's'
    return $ RotateDir (dir op1) op2

movP :: Parser Instruction
movP = do
  void $ string "move position "
  op1 <- numberP
  void $ string " to position "
  op2 <- numberP
  return $ Move op1 op2

revP :: Parser Instruction
revP = do
  void $ string "reverse positions "
  op1 <- numberP
  void $ string " through "
  op2 <- numberP
  return $ Reverse op1 op2


numberP :: Parser Int
numberP = read <$> (negNumber <|> number)
  where
    negNumber = (:) <$> char '-' <*> number
    number = many1 digit

dir :: String -> Dir
dir "left" = L
dir "right" = R

scramble :: String -> [Instruction] -> String
scramble = foldl' apply

unscramble :: String -> [Instruction] -> String
unscramble s is = fromMaybe "NO MATCH FOUND" $ find f candidates
  where
    f s' = s == scramble s' is
    candidates = permutations s

apply :: String -> Instruction -> String
apply s (SwapPos p0 p1) = replaceAt (replaceAt s p0 c1) p1 c0
  where
    c0 = s !! p0
    c1 = s !! p1
apply s (SwapLet c0 c1) = (replace '?' c0 . replace c0 c1 . replace c1 '?') s
apply s (RotatePos c) = apply s (RotateDir R amt)
  where
    idx = fromMaybe (error "bad char") $ c `elemIndex` s
    amt = 1 + idx + (if idx >= 4 then 1 else 0)
apply s (RotateDir d c) = case d of
  R -> rotL s (length s - c)
  L -> rotL s c
apply s (Move p0 p1) = insertAt (h ++ tail t) p1 (head t)
  where
    (h, t) = splitAt p0 s
apply s (Reverse p0 p1) = before ++ reverse target ++ after
  where
    (before, rest) = splitAt p0 s
    (target, after) = splitAt (1 + p1 - p0) rest

rotL :: [a] -> Int -> [a]
rotL s c = t ++ h
  where
    (h, t) = splitAt (c `mod` length s) s

replaceAt :: [a] -> Int -> a -> [a]
replaceAt xs p x = xs0 ++ [x] ++ (tail xs1)
  where
    (xs0, xs1) = splitAt p xs

insertAt :: [a] -> Int -> a -> [a]
insertAt xs p x = xs0 ++ [x] ++ xs1
  where
    (xs0, xs1) = splitAt p xs

replace :: Eq a => a -> a -> [a] -> [a]
replace x0 x1 = map (\x -> if x == x0 then x1 else x)
