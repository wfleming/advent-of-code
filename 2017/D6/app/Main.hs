module Main where

import D6Lib
import Text.Megaparsec
import Text.Megaparsec.String
import System.Environment (getArgs)

parseFile :: Parser Mem -> String -> IO ParseResult
parseFile p path = do
  contents <- readFile path
  return $ parse p path contents

-- Is this really the best way to do this? I thought I could `lift` from one
-- monad to the other somehow, but couldn't get it working
eitherMap :: Either a b -> (b -> Maybe c) -> Maybe c
eitherMap (Left _) _ = Nothing
eitherMap (Right x) f = f x

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFile parser file
  let states = reallocateUntilDupe . (replicate 1) <$> parseResult
  let stepCount = (+ (-1)) . length <$> states
  putStrLn $ "p1: iterations until dupe seen = " ++ (show stepCount)
  let cLength = eitherMap states cycleLength
  putStrLn $ "p2: cycle length = " ++ (show cLength)
