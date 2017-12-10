module D9Lib  where

scrubGarbage :: String -> String
scrubGarbage = scrub' False False
  where
    --     inGarbage -> skipNext -> input -> output
    scrub' :: Bool -> Bool -> String -> String
    scrub' _ _ [] = []
    scrub' inGarbage True (h : t) = scrub' inGarbage False t
    scrub' True _ ('>' : t) = scrub' False False t
    scrub' True _ ('!' : t) = scrub' True True t
    scrub' True _ (h : t) = scrub' True False t
    scrub' False _ ('<' : t) = scrub' True False t
    scrub' False _ (h : t) = h : scrub' False False t

scoreGroups :: String -> Int
scoreGroups = score' 0 0
  where
    --        totScore -> parentScore -> input -> outScore
    score' :: Int -> Int -> String -> Int
    score' s _ [] = s
    score' s ps ('{' : t) = score' (s + ps + 1) (ps + 1) t
    score' s ps ('}' : t) = score' s (ps - 1) t
    score' s ps (_ : t) = score' s ps t
