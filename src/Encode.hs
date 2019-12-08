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