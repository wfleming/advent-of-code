module D10Lib where

import Control.Monad
import Data.List
import Data.Maybe
import Text.Parsec hiding (Error, parse)
import Text.Parsec.String

data Dest = DestBot Int | DestOutput Int deriving (Eq, Show)

data AST =
      ASTValue Int Int -- value X goes to bot Y
    | ASTBot Int Dest Dest -- bot X gives low to OUT Y, high to OUT Z
    deriving (Eq, Show)

data Bot = Bot
    { num :: Int
    , c1 :: Maybe Int
    , c2 :: Maybe Int
    }
    deriving (Eq, Show)

instance Ord Bot where
    compare n1 n2 = compare (num n1) (num n2)

rulesP :: Parser [AST]
rulesP = many ruleP

ruleP :: Parser AST
ruleP = do
    r <- botRuleP <|> valRuleP
    char '\n'
    return $ r

botRuleP :: Parser AST
botRuleP = do
    void $ string "bot "
    b <- number
    void $ string " gives low to "
    ld <- destIdP
    void $ string " and high to "
    hd <- destIdP
    return $ ASTBot b ld hd

valRuleP :: Parser AST
valRuleP = do
    void $ string "value "
    v <- number
    void $ string " goes to bot "
    b <- number
    return $ ASTValue v b

destIdP :: Parser Dest
destIdP = do
    d <- destTypeP
    void $ space
    n <- number
    return (d n)

destTypeP :: Parser (Int -> Dest)
destTypeP = p' "bot" DestBot <|> p' "output" DestOutput
  where
    p' :: String -> (Int -> Dest) -> Parser (Int -> Dest)
    p' s t = do
        _ <- string s
        return t

number :: Parser Int
number = read <$> many1 digit

-- just the ASTValue rules
initRules :: [AST] -> [AST]
initRules = filter isInitRule
  where
    isInitRule (ASTValue _ _) = True
    isInitRule _ = False

-- just the ASTBot rules
botRules :: [AST] -> [AST]
botRules = filter isBotRule
  where
    isBotRule (ASTBot _ _ _) = True
    isBotRule _ = False

-- if possible, find a card a bot will receive it doesn't already know
-- about, and return the card num
backtrackCard :: [AST] -> [Bot] -> Bot -> Maybe Int
backtrackCard rs bs b = listToMaybe sourceCards
  where
    sourceBotRs = filter isSourceRule rs
    isSourceRule (ASTBot _ (DestBot bn) _) | bn == num b = True
    isSourceRule (ASTBot _ _ (DestBot bn)) | bn == num b = True
    isSourceRule _ = False
    sourceBots = (filter bhasSourceR . filter full) bs
    bhasSourceR Bot { num = bn } = any (\(ASTBot rbn _ _) -> bn == rbn) sourceBotRs
    sourceCards = (filter (not . cardOnBot b) . map cardFor) sourceBots
    -- return the card `sb` here would give to `b` given to `backtrackCard`
    cardFor sb = case (sb, sbr) of
        ((Bot { c1 = Just c1' }), (ASTBot _ (DestBot b') _)) | b' == (num b) -> c1'
        ((Bot { c2 = Just c2' }), (ASTBot _ _ (DestBot b'))) | b' == (num b) -> c2'
        _ -> error "come on now"
      where
        sbr = fromMaybe (error "nope") $ find (\(ASTBot rbn _ _) -> (num sb) == rbn) sourceBotRs

cardOnBot :: Bot -> Int -> Bool
cardOnBot b c = (Just c) == (c1 b) || (Just c) == (c2 b)

graphFromAST :: [AST] -> [Bot]
graphFromAST rules =  until listFull (iter (botRules rules)) (startBots rules)
  where
    listFull = all full
    iter :: [AST] -> [Bot] -> [Bot]
    iter rs bs = foldl' (f rs bs) [] bs
    f :: [AST] -> [Bot] -> [Bot] -> Bot -> [Bot]
    f rs allBots memo b | full b = b : memo
    f rs allBots memo b = case backtrackCard rs allBots b of
        Just c -> (addCard b c) : memo
        _ -> b : memo

emptyBots :: [AST] -> [Bot]
emptyBots = (map f) . botRules
  where
    f (ASTBot b _ _) = Bot { num = b, c1 = Nothing, c2 = Nothing }

-- fill in bots that get cards from the beginning
startBots :: [AST] -> [Bot]
startBots rules = foldl' f (emptyBots rules) (initRules rules)
  where
    f bs (ASTValue c bnum) = replace bs (addCard (bot bs bnum) c)
    bot bs b = fromMaybe (error "invalid bot num") $ find ((== b) . num) bs

addCard :: Bot -> Int -> Bot
addCard b@Bot { c1 = Nothing, c2 = Nothing } c = Bot
    { num = num b
    , c1 = Just c
    , c2 = Nothing
    }
addCard b@Bot { c1 = Just c1', c2 = Nothing } c = Bot
    { num = num b
    , c1 = Just $ min c c1'
    , c2 = Just $ max c c1'
    }
addCard _ _ = error "trying to add a card to a full bot"

full :: Bot -> Bool
full Bot { c1 = Just _, c2 = Just _ } = True
full _ = False

replace :: [Bot] -> Bot -> [Bot]
replace (h:t) b = if num h == num b
    then b:t
    else h : (replace t b)
replace [] _ = []

findBot :: [Bot] -> Int -> Bot
findBot bots b = fromMaybe (error "invalid bot id") $ find ((== b) . num) bots

-- find the rule defining a bots actions
botRule :: [AST] -> Int -> AST
botRule rules bNum = fromMaybe (error "bad bot num") $ find f rules
  where
    f (ASTBot b _ _) = b == bNum
    f _ = False

-- determine which card ends up in a given output bucket
outputCard :: [AST] -> [Bot] -> Int -> Int
outputCard rs bs o = case brule of
    (ASTBot _ (DestOutput o') _) | o' == o -> fromJust $ c1 bot
    (ASTBot _ _ (DestOutput o')) | o' == o -> fromJust $ c2 bot
  where
    brule = fromMaybe (error "no rule") $ find bruleTest rs
    bruleTest (ASTBot _ (DestOutput o') _) | o' == o = True
    bruleTest (ASTBot _ _ (DestOutput o')) | o' == o = True
    bruleTest _ = False
    bot = case brule of
        (ASTBot bnum _ _) -> findBot bs bnum
        _ -> error "brule failed"
