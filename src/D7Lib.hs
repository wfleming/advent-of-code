module D7Lib where

import Data.List
import Data.Maybe
import Util

-- True if in brackets, seenMatch, and last 4 chars seen
type TLSContext = (Bool, Bool, Maybe Char, Maybe Char, Maybe Char, Maybe Char)

data SSLContext = SSLContext
    { inBraces :: Bool
    , abas :: [(Char, Char, Char)]
    , babs :: [(Char, Char, Char)]
    , lastChars :: (Maybe Char, Maybe Char)
    } deriving (Show)

emptyTLSContext = (False, False, Nothing, Nothing, Nothing, Nothing)

emptySSLContext = SSLContext False [] [] (Nothing, Nothing)

supportsTLS :: TLSContext -> String -> Bool
supportsTLS (False, False, c1, c2, c3, c4) xs
    | c1 == c4 && c2 == c3 && c1 /= c3
    = supportsTLS (False, True, c1, c2, c3, c4) xs
supportsTLS (True, _, c1, c2, c3, c4) _
    | c1 == c4 && c2 == c3 && c1 /= c3
    = False
supportsTLS (_, sawMatch, _, _, _, _) [] = sawMatch
supportsTLS (inBraces, sawMatch, c1, c2, c3, c4) (h:t)
    = supportsTLS (inBraces', sawMatch, c2, c3, c4, trackChar) t
  where
    inBraces' = if h == '[' || h == ']'
        then not inBraces
        else inBraces
    trackChar = if h == '[' || h == ']'
        then Nothing
        else Just h

supportsSSL :: SSLContext -> String -> Bool
supportsSSL ctx []
    | SSLContext _ ctxAbas ctxBabs _ <- ctx
    = not . null $ ctxBabs `intersect` map abaToBab ctxAbas
  where
    abaToBab (c1, c2, _) = (c2, c1, c2)
supportsSSL ctx (h:t) =
    supportsSSL ctx' t
  where
    inBraces' = if h == '[' || h == ']'
        then not $ inBraces ctx
        else inBraces ctx
    trackChar = if h == '[' || h == ']'
        then Nothing
        else Just h
    (c1, c2) = lastChars ctx
    lastChars' = (c2, trackChar)
    thisTrip = if c1 == trackChar && c2 /= c1 && isJust c1 && isJust c2
        then Just (fromMaybe wtf c1, fromMaybe wtf c2, fromMaybe wtf c1)
        else Nothing
    abas' = if not (inBraces ctx) && isJust thisTrip
        then fromMaybe wtf thisTrip : abas ctx
        else abas ctx
    babs' = if inBraces ctx && isJust thisTrip
        then fromMaybe wtf thisTrip : babs ctx
        else babs ctx
    ctx' = SSLContext inBraces' abas' babs' lastChars'
    wtf = error "wtf"

countIpsSupportingTLS :: [String] -> Int
countIpsSupportingTLS = length . filter (supportsTLS emptyTLSContext)

countIpsSupportingSSL :: [String] -> Int
countIpsSupportingSSL = length . filter (supportsSSL emptySSLContext)

