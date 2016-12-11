module D9Lib where

import Control.Monad
import Data.Char
import Data.List
import Text.Parsec hiding (Error)
import Text.Parsec.String

decompressV1 :: Parser String
decompressV1 = parsecMap concat $ many (markerParser <|> anyCharAsStr)

markerParser :: Parser String
markerParser  = do
    (l, r, t) <- marker
    return $ (concat . replicate r) t

-- parse a decompress marker, return the details of the marker itself & the text
-- to decompress following the marker
marker :: Parser (Int, Int, String)
marker = do
    void $ char '('
    l <- number
    void $ char 'x'
    r <- number
    void $ char ')'
    t <- count l anyChar
    return (l, r, t)

anyCharAsStr :: Parser String
anyCharAsStr = do
    c <- anyChar
    return [c]

number :: Parser Int
number = read <$> many1 digit

stripAll :: String -> String
stripAll = filter $ not . isSpace

-- The messages here can get VERY large: we only want length, so don't build
-- decompressed msg in mem, just track lengths
decompressV2 :: Parser Int
decompressV2 = parsecMap sum $ many (markerParserV2 <|> anySubStrLen)

markerParserV2 :: Parser Int
markerParserV2  = do
    (l, r, t) <- marker
    let pr = parse decompressV2 "SUB-PARSE" t
    let expandedLength = either (error . show) id pr
    return $ expandedLength * r

anySubStrLen :: Parser Int
anySubStrLen = do
    c <- anyChar
    return $ if isSpace c then 0 else 1
