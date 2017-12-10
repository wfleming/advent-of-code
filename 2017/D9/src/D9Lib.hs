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

countGarbage :: String -> Int
countGarbage = scrub' 0 False False
  where
    --     inGarbage -> skipNext -> input -> output
    scrub' :: Int -> Bool -> Bool -> String -> Int
    scrub' memo _ _ [] = memo
    scrub' memo inGarbage True (h : t) = scrub' memo inGarbage False t
    scrub' memo True _ ('>' : t) = scrub' memo False False t
    scrub' memo True _ ('!' : t) = scrub' memo True True t
    scrub' memo True _ (h : t) = scrub' (memo + 1) True False t
    scrub' memo False _ ('<' : t) = scrub' memo True False t
    scrub' memo False _ (h : t) = scrub' memo False False t
