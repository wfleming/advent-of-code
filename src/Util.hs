module Util where

import Crypto.Hash
import Data.List
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Lazy.Char8 as LC

type Freq = (Char, Int)

{- | Determine the frequency of each char in a string. -}
freq :: String -> [Freq]
freq = map charfreq . group . sort . filter (/= '-')
  where
    charfreq l | h : _ <- l = (h, length l)
    charfreq _ = error "empty freq list?"

{- | Simple wrapper to get the MD5 hash of a String as a String -}
md5 :: String -> String
md5 = LC.unpack . LC.fromStrict . digestToHexByteString . md5' . LC.pack
  where
    md5' :: LB.ByteString -> Digest MD5
    md5' = hashlazy

{- | fn to be shorthand for a `seq` a -}
seq' :: a -> a
seq' a' = a' `seq` a'
