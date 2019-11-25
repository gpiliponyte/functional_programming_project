module BattleshipJSON

where
import Data.Char
import Data.List

data Result = Hit | Miss deriving (Eq,Show)
data Player = A | B deriving (Eq,Show)

data Game = Game (Int,Int) Result (Maybe Game) deriving (Eq,Show)
data GameString = GameString String String String deriving (Eq,Show)

points :: Game -> ((Int,Int),Player)
points (Game _ Hit Nothing) = ((1,0),B)
points (Game _ Miss Nothing) = ((0,0),B)
points (Game _ Hit (Just p))
    | pl == A = ((a+1,b),B)
    | pl == B = ((a,b+1),A)
    where
        ((a,b),pl) = points p
points (Game _ Miss (Just p))
    | pl == A = ((a,b),B)
    | pl == B = ((a,b),A)
    where
        ((a,b),pl) = points p

appendFst:: Char -> (String,String) -> (String,String)
appendFst c (a,b) = (c:a,b)

findBracket:: String -> (String,String)
findBracket ('[':str) = findEnd' 1 str
    where
        findEnd' n [] = error "list with no end"
        findEnd' 1 (']':s) = ("",s)
        findEnd' n ('[':s) = appendFst '[' $ findEnd' (n+1) s
        findEnd' n (']':s) = appendFst ']' $ findEnd' (n-1) s
        findEnd' n (c:s) = appendFst c $ findEnd' n s

findComma:: String -> (String,String)
findComma (',':str) = findComma' 0 str
    where
        findComma' 0 [] = ("","")
        findComma' n [] = error ""
        findComma' 0 (',':s) = ("",s)
        findComma' n ('[':s) = appendFst '[' $ findComma' (n+1) s
        findComma' n (']':s) = appendFst ']' $ findComma' (n-1) s
        findComma' n (c:s) = appendFst c $ findComma' n s


prefix:: String -> String -> (Bool,String) 
prefix [] s = (True,s)
prefix p [] = (False,"")
prefix (a:p) (b:s) 
    | a==b = prefix p s
    | otherwise = (False,"")

replaceAt:: [a] -> Int -> a -> [a]
replaceAt [] _ _ = []
replaceAt (_:xs) 0 r = r:xs
replaceAt (x:xs) n r 
    | n>0 = replaceAt xs (n-1) r
    | n<0 = (x:xs)

generateBoard:: Int -> [[Bool]]
generateBoard n = generateBoard' n
    where
        generateBoard' 0 = []
        generateBoard' i = l : (generateBoard' (i-1))
        l = generateList n
        generateList 0 = []
        generateList i = False : (generateList (i-1))

applyMove:: Int -> Int -> [[Bool]] -> [[Bool]]
applyMove n m board = replace (applyList m $ board!!n) n board
    where
        applyList 0 (_:xs) = (True:xs)
        applyList n (x:xs) = x:(applyList (n-1) xs)
        replace list 0 (_:ls) = list:ls
        replace list n (l:ls) = l:(replace list (n-1) ls)

score:: String -> Either String (Int,Int)
score str
    | isGame game = Right $ fst $ points $ right game
    | otherwise = Left $ left game
    where
        game = createGame str board board True
        right (Right r) = r
        left (Left l) = l
        board = generateBoard 10

isGame:: Either String Game -> Bool
isGame (Right _) = True
isGame (Left _) = False

maybeGet:: Maybe a -> a
maybeGet (Just a) = a

createGameString:: String -> [(Int,String)]
createGameString "" = []
createGameString str
    | not prc && not prs && not prp = [(-1,"doesn't match any field")]
    | prc = (0,cc1) : (createGameString cc2)
    | prs = (1,cs1) : (createGameString cs2)
    | prp = (2,cp1) : (createGameString cp2)
    where
        (prc,sfc) = prefix "\"coord\"" str 
        (cc1,cc2) = findComma sfc
        (prs,sfs) = prefix "\"result\"" str
        (cs1,cs2) = findComma sfs
        (prp,sfp) = prefix "\"prev\"" str
        (cp1,cp2) = findComma sfp



coordCheck:: String -> (Int,Int)
coordCheck str
    | b1 && r1/="" && b2 && r3/="" && b3 && r4=="\"]" && isAsciiUpper ch && n<75 = (n-65,9)
    | b1 && r1/="" && b2 && r3/="" && (isAsciiUpper ch) && (isDigit ch2) && r5=="\"]" && n<75 = (n-65,(digitToInt ch2)-1)
    | otherwise = (-1,-1)
    where 
        (b1,r1) = prefix "[\"" str
        ch = head r1
        n = ord ch
        r2 = tail r1
        (b2,r3) = prefix "\",\"" r2
        (b3,r4) = prefix "10" r3
        ch2 = head r3
        r5 = tail r3

resultCheck:: String -> Maybe Result
resultCheck "\"HIT\"" = Just Hit
resultCheck "\"MISS\"" = Just Miss
resultCheck _ = Nothing

createGame:: String -> [[Bool]] -> [[Bool]] -> Bool -> Either String Game
createGame str b1 b2 pl
    | length l < 2 = Left "not enough fields defined"
    | length l >= 4 = Left "too many fields defined"
    | fst h1 == -1 = Left $ snd h1
    | fst h1 /= 0 = Left "no coord defined"
    | fst h1 == 0 && j<0 = Left "error with given coordinates"
    | fst h1 == 0 && pl && b1!!i!!j = Left "shot twice in one coordinate"
    | fst h1 == 0 && not pl && b2!!i!!j = Left "shot twice in one coordinate"
    | fst h1 == 0 && fst h2 == 0 = Left "two or more coords defined"
    | fst h1 == 0 && fst h2 /= 1 = Left "no result defined"
    | fst h1 == 0 && fst h2 == 1 && shot == Nothing = Left "result doesn't match possible results"
    | fst h1 == 0 && fst h2 == 1 && t2 == [] = Right (Game shotcoord (maybeGet shot) Nothing)
    | fst h1 == 0 && fst h2 == 1 && t2 == [] && not pl && b2!!i!!j = Left "shot twice in one coordinate"
    | fst h1 == 0 && fst h2 == 1 && fst h3 == 1 = Left "two or more results defined"
    | fst h1 == 0 && fst h2 == 1 && fst h3 == 2 && pl && isGame prev1 = Right (Game shotcoord (maybeGet shot) (Just $ right prev1))
    | fst h1 == 0 && fst h2 == 1 && fst h3 == 2 && pl = prev1
    | fst h1 == 0 && fst h2 == 1 && fst h3 == 2 && not pl && isGame prev2 = Right (Game shotcoord (maybeGet shot) (Just $ right prev2))
    | fst h1 == 0 && fst h2 == 1 && fst h3 == 2 && not pl = prev2 
    where
        right (Right r) = r
        (s,r) = findBracket str
        l = sort $ createGameString s
        h1 = head l
        t1 = tail l
        h2 = head t1
        t2 = tail t1
        h3 = head t2
        shotcoord@(i,j) = coordCheck $ snd h1
        shot = (resultCheck $ snd h2)
        prev1 = createGame (snd h3) (applyMove i j b1) b2 False
        prev2 = createGame (snd h3) b1 (applyMove i j b2) True