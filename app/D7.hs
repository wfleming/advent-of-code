module Main (main) where

import D7Lib
import System.Environment (getArgs)

main :: IO ()
main = do
  file <- head <$> getArgs
  fileLines <- lines <$> readFile file
  let tlsNum = countIpsSupportingTLS fileLines

  putStrLn $ "number of IPS supporting TLS: " ++ show tlsNum

  let sslNum = countIpsSupportingSSL fileLines

  putStrLn $ "number of IPS supporting SSL: " ++ show sslNum
