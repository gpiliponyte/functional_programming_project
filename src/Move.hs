module Move where

import Encode
import Decode

data CellStatus = N | HS | S | M | U deriving (Show, Eq)

myBoard:: [[CellStatus]]
myBoard = [[S, S, S, S, N, N, S, S, N, N],
    [N, N, N, N, N, N, N, N, N, N],
    [S, S, N, N, N, N, S, S, N, N],
    [N, N, N, N, N, N, N, N, N, N],
    [N, N, N, N, N, N, N, N, N, N],
    [N, S, S, S, N, S, N, S, N, N],
    [N, N, N, N, N, N, N, N, N, N],
    [N, N, N, N, N, N, N, N, N, N],
    [N, N, N, S, S, S, N, N, S, N],
    [N, N, N, N, S, N, N, N, N, N]]

opponentsBoard::[[CellStatus]]
opponentsBoard = [[U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U],
    [U, U, U, U, U, U, U, U, U, U]]


getAMove:: [[CellStatus]] -> Either String (String, String)
getAMove cells = 
    case findAvailableCell cells of
        Just pair -> Right (numCoordToStringCoord pair)
        Nothing -> Left "Could not find available moves"


findAvailableCell:: [[CellStatus]] -> Maybe (Int, Int)
findAvailableCell [s] = findAvailableCell' s 10 1
findAvailableCell cells@(xs:s) = 
    case findAvailableCell' xs (11 - (length cells)) 1 of
        Just (n, m) -> Just (n, m)
        Nothing -> findAvailableCell s

findAvailableCell':: [CellStatus] -> Int -> Int-> Maybe (Int, Int)
findAvailableCell' [] _ _ = Nothing
findAvailableCell' (xs: s) n m =
    case xs of
        U -> Just (n, m)
        _ -> findAvailableCell' s n (m + 1)

numCoordToStringCoord:: (Int, Int) -> (String, String)
numCoordToStringCoord (x, y) = (numToString x, show y)

numToString:: Int -> String
numToString 1 = "A"
numToString 2 = "B"
numToString 3 = "C"
numToString 4 = "D"
numToString 5 = "E"
numToString 6 = "F"
numToString 7 = "G"
numToString 8 = "H"
numToString 9 = "I"
numToString 10 = "J"
numToString _ = ""