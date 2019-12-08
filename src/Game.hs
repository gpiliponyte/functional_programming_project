module Game where

    import Network.HTTP.Client
    import Network.HTTP.Types.Status (statusCode)
    import System.Environment
    import Data.ByteString.Internal (unpackBytes)
    import Data.ByteString.Char8 (pack, unpack)
    import qualified Data.ByteString.Lazy as L
    import qualified Data.ByteString.Char8 as BS
    import qualified Data.CaseInsensitive as CI
    import GHC.Word (Word8)
    import Control.Lens

    import Move
    import Decode
    import Encode
    import Available

    baseUrl = "http://battleship.haskell.lt/game/"

    formatStringAfterGet :: String -> String
    formatStringAfterGet history = 
        case parseStringToMessage history of
            Right gameMessage -> "l6:result" ++ (getResultString myBoard history)  ++"4:prev" ++ history ++ "e"
            Left e -> ""

    formatStringBeforePost :: String -> (String, String) -> String
    formatStringBeforePost "" (x, y) = "l5:coordl" ++ (show (length x)) ++ ":" ++ x ++ (show (length y)) ++ ":" ++ y ++ "ee" 
    formatStringBeforePost history (x, y) = 
        case parseStringToMessage history of
            Right gameMessage -> "l5:coordl" ++ (show (length x)) ++ ":" ++ x ++ (show (length y)) ++ ":" ++ y ++ "e" ++ firstLast history ++ "e"
            Left e -> ""


    getResultString :: [[CellStatus]] -> String -> String
    getResultString board history = 
        case parseStringToMessage history of
            Right message -> show resultLength ++ ":" ++ result
                where 
                    (x, y) = (stringCoordToNumbers (coord message))
                    cellValue = board!!(x - 1)!!(y - 1)
                    result = getResult cellValue
                    resultLength = length result
            Left e -> "4:MISS"


    startGame :: String -> String -> IO()
    startGame gameId gameVariant = do
        manager <- newManager defaultManagerSettings
        case gameVariant of 
            "A" -> 
                playGame gameId opponentsBoard "" manager "A" 1
            "B" -> do
                history <- (getMoves manager gameId "B")
                let updatedUpdatedHistory = formatStringAfterGet (firstLast history)
                case parseStringToMessage updatedUpdatedHistory of
                    Right gameMessage -> playGame gameId opponentsBoard updatedUpdatedHistory manager "B" 1
                    Left e -> putStrLn e

    getBoard :: [[CellStatus]] -> (Int, Int) -> [[CellStatus]]
    getBoard board (x, y) = (take (x - 1) board) ++ [(getRow (board!!(x-1)) y)] ++ (drop x board)

    getRow :: [CellStatus] -> Int -> [CellStatus]
    getRow row y = (take (y - 1) row) ++ [M] ++ (drop y row)


    getMessageWithResult:: Message -> [[CellStatus]] -> Message
    getMessageWithResult Message {coord = (x, y), prev = prevMsg} board = Message {coord = (x, y), prev = prevMsg, result = moveResult}
        where 
            moveResult = Just Hit -- to do : implement logic

    getUpdatedString :: [[CellStatus]] -> String -> String
    getUpdatedString board history = 
        case parseStringToMessage history of
            Right message -> ((take (length history -1 ) history) ++ "6:result" ++ show resultLength ++ ":" ++ result ++"e")
                where 
                    (x, y) = (stringCoordToNumbers (coord message))
                    cellValue = board!!(x -1)!!(y - 1)
                    result = getResult cellValue
                    resultLength = length result
            Left e -> "Something went wrong"

    getResult :: CellStatus -> String
    getResult cell = if cell == N
        then "MISS"
        else "HIT"


    playGame :: String -> [[CellStatus]] -> String -> Manager -> String -> Int -> IO()
    playGame gameId opGrid history manager var i = do
        case getAMove opGrid of 
            Right nextMove -> do
                let strToSend = formatStringBeforePost history nextMove
                let j = i + 1
                putStrLn (show nextMove)
                postMoves manager gameId strToSend var; -- siunciu
                receivedHistory <- (getMoves manager gameId var); -- gaunu atsakyma
                let updatedHistory = firstLast receivedHistory -- apdoroju atsakyma IR TURECIAU PARASYT, AR HIT ANAS MOVE
                case parseStringToMessage updatedHistory of
                    Right receivedMsg ->
                        case (coord receivedMsg) of
                            ("0", "0") -> putStrLn ("Player " ++ show var ++ " won")
                            _ -> do
                                putStrLn (show (coord receivedMsg))
                                let updatedUpdatedHistory = formatStringAfterGet updatedHistory --getUpdatedString myBoard updatedHistory
                                case parseStringToMessage updatedUpdatedHistory of
                                    Right gameMessage -> 
                                        case isGameOver gameMessage of
                                            Right True -> declareThatIlost gameId updatedUpdatedHistory var manager
                                            Right False -> 
                                                case (prev gameMessage) of
                                                    Just msg -> 
                                                        case (prev msg) of
                                                            Just m -> --playGame gameId (getBoard opGrid (stringCoordToNumbers (coord m))) updatedUpdatedHistory manager var j
                                                                case i of 
                                                                -- _ -> putStrLn (show (coord m))
                                                                _ -> playGame gameId (getBoard opGrid (stringCoordToNumbers (coord m))) updatedUpdatedHistory manager var j
                                                            Nothing -> putStrLn "Something bad happened"
                                                    Nothing -> putStrLn "Something bad happened"
                                            Left e -> putStrLn e
                                    Left e -> putStrLn e
                    Left e -> putStrLn "somtehing went wrong"
            Left e -> putStrLn "Something went wrong when getting a move"

    declareThatIlost :: String -> String -> String ->  Manager -> IO()
    declareThatIlost gameId history var manager = do
        let strToSend = "l5:coordle" ++ firstLast history ++ "e"
        putStrLn ("Player " ++ (show (if var == "A" then "B" else "A")) ++ " won")
        postMoves manager gameId strToSend var


    getMoves :: Manager -> String -> String -> IO(String)
    getMoves manager gameId var = do
        getRequestUrl <- parseUrl $ baseUrl ++ gameId ++ "/player/" ++ var 
        let request = getRequestUrl {
            method = stringToBS $ "GET"
        , requestHeaders = [(makeCaseInsensitive $ stringToBS $ "Accept", stringToBS $ "application/relaxed-bencoding+nomaps")] }
    
        response <- httpLbs request manager
        putStrLn "GET:"
        -- putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
        
        return $ show $ responseBody response

    postMoves :: Manager -> String -> String -> String -> IO()
    postMoves manager gameId gameStr var = do
        postRequestUrl <- parseUrl $ baseUrl ++ gameId ++ "/player/" ++ var
        let request = postRequestUrl { 
            method = stringToBS $ "POST"
        , requestBody = RequestBodyLBS $ word8ToByteString $ strToWord8s $ gameStr
        , requestHeaders = [(makeCaseInsensitive $ stringToBS $ "Content-Type", stringToBS $ "application/relaxed-bencoding+nomaps")] }
    
        response <- httpLbs request manager
        Prelude.putStrLn "POST"
        -- Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)

    strToWord8s :: String -> [Word8]
    strToWord8s = unpackBytes . pack
    
    word8ToByteString :: [Word8] -> L.ByteString
    word8ToByteString a = L.pack a

    stringToBS :: String -> BS.ByteString  -- refers to lazy ByteStrings
    stringToBS str = BS.pack str
    
    makeCaseInsensitive :: BS.ByteString -> CI.CI BS.ByteString
    makeCaseInsensitive byteStr = CI.mk byteStr

    firstLast::[a]->[a]
    firstLast [] = []
    firstLast [x] = []
    firstLast xs = tail (init xs)

    stringCoordToNumbers:: (String, String) -> (Int, Int)
    stringCoordToNumbers (x, y) = (letterToNum x, numStrToNum y)

    numStrToNum:: String -> Int
    numStrToNum "1" = 1
    numStrToNum "2" = 2
    numStrToNum "3" = 3
    numStrToNum "4" = 4
    numStrToNum "5" = 5
    numStrToNum "6" = 6
    numStrToNum "7" = 7
    numStrToNum "8" = 8
    numStrToNum "9" = 9
    numStrToNum "10" = 10
    numStrToNum _ = 0

    letterToNum:: String -> Int
    letterToNum "A" = 1
    letterToNum "B" = 2
    letterToNum "C" = 3
    letterToNum "D" = 4
    letterToNum "E" = 5
    letterToNum "F" = 6
    letterToNum "G" = 7
    letterToNum "H" = 8
    letterToNum "I" = 9
    letterToNum "J" = 10
    letterToNum _ = 0

    -- numStrToNum:: String -> Int
    -- numStrToNum "1" = 0
    -- numStrToNum "2" = 1
    -- numStrToNum "3" = 2
    -- numStrToNum "4" = 3
    -- numStrToNum "5" = 4
    -- numStrToNum "6" = 5
    -- numStrToNum "7" = 6
    -- numStrToNum "8" = 7
    -- numStrToNum "9" = 8
    -- numStrToNum "10" = 9
    -- numStrToNum _ = 0

    -- letterToNum:: String -> Int
    -- letterToNum "A" = 0
    -- letterToNum "B" = 1
    -- letterToNum "C" = 2
    -- letterToNum "D" = 3
    -- letterToNum "E" = 4
    -- letterToNum "F" = 5
    -- letterToNum "G" = 6
    -- letterToNum "H" = 7
    -- letterToNum "I" = 8
    -- letterToNum "J" = 9
    -- letterToNum _ = 0

