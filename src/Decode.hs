module Decode where

    import Data.Char
    import Data.List
    
    data Result = Miss | Hit deriving (Show)
    
    data Message = Message {
        coord :: (String, String),
        result :: Maybe Result,
        prev :: Maybe Message
    } deriving (Show)
    
    emptyMessage = Message {coord = ("0", "0"), result = Nothing, prev = Nothing}
    
    parseString :: String -> Either String (String, String)
    parseString msg = readStr lenght $ drop (length lenghtAsStr) msg
        where
            lenghtAsStr = takeWhile isDigit msg
            lenght = read lenghtAsStr
            readStr :: Int -> String -> Either String (String, String)
            readStr n (':':m) = Right (take n m, drop n m)
            readStr _ str = Left "Key could not be parsed"
    
    parseStringToMessage :: String -> Either String Message
    parseStringToMessage str =
        case result of
            Left errorMsg -> Left errorMsg
            Right (message, "") -> Right message
            _ -> Left "Game string must have only one element that is an array at the top level"
        where
            result = parseGame str
    
    
    parseGame ('l' : gameString) = parseMove gameString emptyMessage
        where
            parseMove :: String -> Message -> Either String (Message, String)
            parseMove ('e' : restOfGame) message = Right (message, restOfGame)
            parseMove moveString message =
                case parseString moveString of
                    Left parseErr -> Left parseErr
                    Right (key, rest) ->
                        case parseByKey key rest message of
                            Left parseKeyErr -> Left parseKeyErr
                            Right (updatedMessage, restOfGameString) -> parseMove restOfGameString updatedMessage
    parseGame str = Left "Letter L expected in the beginning of a game element"
    
    parseByKey :: String -> String -> Message -> Either String (Message, String)
    parseByKey key str message =
        case key of
            "coord" -> 
                case processCoordinates str of
                    Left e -> Left e
                    Right (coord1, coord2, rest) -> Right (message {coord = (coord1, coord2)}, rest)
            "result" -> Right (message, str)
            "prev" -> 
                case parseGame str of
                    Right (previousMessage, restStr) -> Right (message {prev = Just previousMessage}, restStr)
                    Left e -> Left e
            "HIT" -> Right (message {result = Just Hit}, str)
            "MISS" -> Right (message {result = Just Miss}, str)
            _ -> Left $ key ++ " is not a valid key"
    
    processCoordinates :: String -> Either String (String, String, String)
    processCoordinates ('l':'e':coordStr) = Right ("0", "0", coordStr)
    processCoordinates ('l': coordStr) = 
        case parseString coordStr of
            Left e -> Left e
            Right (coord1, rest1) -> 
                case parseString rest1 of
                    Left e -> Left e
                    Right (coord2, 'e':rest2) -> Right (coord1, coord2, rest2)
                    _ -> Left "coordinates must end with an e"
    processCoordinates _ = Left "L was expected in coordinate beginning"