module Encode where 

import Decode
import qualified Data.Char

encodeMoveToBencodeWithoutMaps :: Message -> String -> String
encodeMoveToBencodeWithoutMaps Message {prev = Nothing, result = Nothing, coord = (x, y)} "" = concat ["l5:coordl" , show (length x), ":", x, show (length y), ":", y, "ee"]
encodeMoveToBencodeWithoutMaps Message {prev = Nothing, result = Just rez, coord = (x, y)} "" = concat ["l5:coordl" , show (length x), ":",x, show (length y), ":", y, "6:result", getResultLenght rez , ":", show rez, "ee"]
encodeMoveToBencodeWithoutMaps Message {prev = Just previous, result = Just rez, coord = (x, y)} history = concat ["l5:coordl" , show (length x), ":", show (length y), ":", y, "6:result", getResultLenght rez, ":", show rez, "4:prev", history, "ee"]
encodeMoveToBencodeWithoutMaps _ _= "Fuck"


convertMessageToMessageArray :: Message -> [Message] -> [Message]
convertMessageToMessageArray m@(Message {prev = Just previousMessage})  messageArray = convertMessageToMessageArray previousMessage messageArray ++ [m]
convertMessageToMessageArray m@(Message {prev = Nothing})  messageArray = foldl (\x y -> y:x) [] (messageArray ++ [m]) 



convertLeWholeMessageHistory :: [Message] -> String -> String
convertLeWholeMessageHistory [] history = "Fuckfuck"
convertLeWholeMessageHistory [s] history = encodeMoveToBencodeWithoutMaps s history
convertLeWholeMessageHistory (xs:s) "" =  convertLeWholeMessageHistory s (encodeMoveToBencodeWithoutMaps xs "")
convertLeWholeMessageHistory (xs:s) history = convertLeWholeMessageHistory s (encodeMoveToBencodeWithoutMaps xs history)

convert :: Message -> String
convert message = convertLeWholeMessageHistory (convertMessageToMessageArray message []) ""

getResultLenght :: Result -> String
getResultLenght result = 
    case result of 
        Hit -> "3"
        Miss -> "4"

getStrToSend:: (String, String) -> String -> String
getStrToSend (x, y) "" = concat ["l5:coordl" , show (length x), ":", x, show (length y), ":", y, "ee"]
getStrToSend (x, y) history = concat ["l5:coordl" , show (length x), ":", x, show (length y), ":", y, "e4:prev",history,"e"]



    -- available :: String -> Either String (Int, Int)
    -- available gameString = 
    --     case parseStringToMessage gameString of
    --         Left parseErr -> Left parseErr
    --         Right gameMessage -> 
    --             case validateForDuplicates gameMessage of 
    --                 Left validationErr -> Left validationErr
    --                 Right x -> getAvailableMoves gameMessage 100 20 100 20 True

    -- getAvailableMoves :: Message -> Int -> Int -> Int -> Int -> Bool -> Either String (Int, Int)
    -- getAvailableMoves message _ 0 _ _ _ = Right (0, 0)
    -- getAvailableMoves message _ _ _ 0 _ = Right (0, 0)
    -- getAvailableMoves Message {prev = Just previousMessage, result = Just Hit} avMov1 avHits1 avMov2 avHits2 True =
    --     getAvailableMoves previousMessage (avMov1 - 1) (avHits1 - 1) avMov2 avHits2 False    -- getAvailableMoves Message {prev = Just previousMessage, result = Just Hit} avMov1 avHits1 avMov2 avHits2 False =
    --     getAvailableMoves previousMessage avMov1 avHits1 (avMov2 - 1) (avHits2 - 1) True
    -- getAvailableMoves Message {prev = Just previousMessage} avMov1 avHits1 avMov2 avHits2 True =
    --     getAvailableMoves previousMessage (avMov1 - 1) avHits1 avMov2 avHits2 False
    -- getAvailableMoves Message {prev = Just previousMessage} avMov1 avHits1 avMov2 avHits2 False =
    --     getAvailableMoves previousMessage avMov1 avHits1 (avMov2 - 1) avHits2 True
    -- getAvailableMoves Message {prev = Nothing, result = Just Hit} avMov1 _ avMov2 _ True = Right (avMov1 - 1, avMov2)
    -- getAvailableMoves Message {prev = Nothing, result = Just Hit} avMov1 _ avMov2 _ False = Right (avMov1, avMov2 -1)
    -- getAvailableMoves Message {prev = Nothing, coord = ("0", "0")} avMov1 _ avMov2 _ _ = Right (avMov1, avMov2)
    -- getAvailableMoves Message {prev = Nothing, coord = (x, y)} avMov1 _ avMov2 _ True = Right (avMov1 - 1, avMov2)
    -- getAvailableMoves Message {prev = Nothing, coord = (x, y)} avMov1 _ avMov2 _ False = Right (avMov1, avMov2 -1)
    -- getAvailableMoves _ _ _ _ _ _ = Left "Unexpected error while obtaining available moves"