module Main (main) where

import D4Lib
import Data.List
import System.Environment (getArgs)
import Text.Parsec.String (parseFromFile)

main :: IO ()
main = do
  file <- head <$> getArgs
  parseResult <- parseFromFile parser file
  let allRooms = either (error . show) id parseResult
  let validRooms = filter valid allRooms
  let sectorSum = sum $ map sector validRooms

  putStrLn $ "There were " ++ (show $ length allRooms) ++ " total rooms"
  putStrLn $ "There were " ++ (show $ length validRooms) ++ " valid rooms"
  putStrLn $ "sum of valid room sectors = " ++ show sectorSum

  let decryptedRooms = map decryptRoom validRooms
  let candidates = filter (\(Room n _ _) -> substring "north" n) decryptedRooms

  putStrLn "Candidate rooms for North Pole storage:"
  putStrLn $ unlines $ map (\(Room n s _) -> n ++ " (" ++ (show s) ++ ")") candidates

substring :: String -> String -> Bool
substring (_:_) [] = False
substring xs ys
    | xs `isPrefixOf` ys = True
    | substring xs (tail ys) = True
    | otherwise = False

