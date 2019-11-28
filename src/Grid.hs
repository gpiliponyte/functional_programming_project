module Grid where

import Data.Char
import Data.List
import Decode

data GridCell = Miss | Hit | Unknown | Clear deriving (Show, Read, Eq)

type Grid = [[GridCell]]

gridSize :: Int
gridSize = 10


emptyGrid :: Grid
emptyGrid = take gridSize (repeat $ take gridSize $ repeat Clear)


makeGrids :: Message -> Either String (Grid, Grid)
makeGrids message =
    case (hasDuplicateMoves list1, hasDuplicateMoves list2) of
        (False, False) -> Right (grid1, grid2)
        _ -> Left "Message contains duplicate moves on 1 cell"
    where
        (reversedList1, reversedList2) = flattenMessageReversed message
        list1 = reverse reversedList1
        list2 = reverse reversedList2
        grid1 = makeGrid list1
        grid2 = makeGrid list2


makeGrid :: [String] -> Grid
makeGrid flatMessageStr = [[findGridCellValue flatMessageStr x y | y <- [1..gridSize]] | x <- [1..gridSize]]


findGridCellValue :: [String] -> Int -> Int -> GridCell
findGridCellValue [] _ _ = Clear
findGridCellValue [coordXStr, coordYStr] x y =
    if (coordXStr, coordYStr) == coords
    then Unknown
    else Clear
    where
        coords = convertCoordIntToStr (x, y)
findGridCellValue (coordXStr : coordYStr : result : rest) x y =
    if (coordXStr, coordYStr) == coords
    then value
    else findGridCellValue rest x y
    where
        coords = convertCoordIntToStr (x, y)
        value :: GridCell
        value = read result


convertCoordIntToStr :: (Int, Int) -> (String, String)
convertCoordIntToStr (coordX, coordY) = (coordXStr, coordYStr)
    where
        coordXStr = numberToLetter coordX
        coordYStr = show coordY


numberToLetter :: Int -> String
numberToLetter number = [chr (number + 64)]


-- recursive Message structure is flattened to a list like this [lastCoordY, lastCoordX, ..., result1, coordY1, coordX1]
flattenMessageReversed :: Message -> ([String], [String])
flattenMessageReversed Message {coord = (coordX, coordY), prev = Nothing} = ([], [coordY, coordX])
flattenMessageReversed Message {coord = (coordX, coordY), result = Just prevRes, prev = Just prevMessage} =
    if lenList2 - lenList1 > 1
    then (coordY : coordX : list1, (show prevRes) : list2)
    else ((show prevRes) : list1, coordY : coordX : list2)
    where
        (list1, list2) = flattenMessageReversed prevMessage
        lenList1 = length list1
        lenList2 = length list2


hasDuplicateMoves :: [String] -> Bool
hasDuplicateMoves (coordX : coordY : result : rest) =
    case hasMove rest coordX coordY of
        True -> True
        False -> hasDuplicateMoves rest
hasDuplicateMoves _ = False


hasMove :: [String] -> String -> String -> Bool
hasMove (coordX : coordY : rest) checkCoordX checkCoordY =
    if (coordX, coordY) == (checkCoordX, checkCoordY)
    then True
    else case rest of
        [] -> False
        _ -> hasMove (tail rest) checkCoordX checkCoordY
hasMove _ _ _ = False