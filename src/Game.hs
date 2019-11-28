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

    import Move
    import Decode
    import Encode
    import Available

    baseUrl = "http://battleship.haskell.lt/game/"

    formatStringAfterGet :: String -> String
    formatStringAfterGet history= 
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
                    cellValue = board!!x!!y
                    result = getResult cellValue
                    resultLength = length result
            Left e -> "4:MISS"


    startGame :: String -> String -> IO()
    startGame gameId gameVariant = do
        manager <- newManager defaultManagerSettings
        case gameVariant of 
            "A" -> 
                playGame gameId opponentsBoard "" manager "A"
            "B" -> do
                history <- (getMoves manager gameId "B")
                let updatedUpdatedHistory = formatStringAfterGet (firstLast history)
                case parseStringToMessage updatedUpdatedHistory of
                    Right gameMessage -> 
                        case isGameOver gameMessage of
                            Right True -> putStrLn "Game completed Successfully"
                            Right False -> 
                                playGame gameId (getNewBoard opponentsBoard 1 [] (stringCoordToNumbers (coord gameMessage))) updatedUpdatedHistory manager "B"
                            Left e -> putStrLn e
                    Left e -> putStrLn e

    getNewBoard:: [[CellStatus]] ->  Int -> [[CellStatus]] -> (Int, Int) -> [[CellStatus]]
    getNewBoard [xs] 10 cell (10, y) = cell ++ [getNewBoard' xs 1 [] y ]
    getNewBoard [xs] _ cell (x, y) = cell ++ [xs]
    getNewBoard (xs: s) i cell (x, y) = 
        if i == x
            then getNewBoard s (x + 1) (cell ++ [getNewBoard' xs 1 [] y]) (x, y)
            else getNewBoard s (i + 1) (cell ++ [xs]) (x, y)

    getNewBoard':: [CellStatus] -> Int -> [CellStatus] -> Int -> [CellStatus]
    getNewBoard' [xs] 10 cell 10 = cell ++ [M]
    getNewBoard' [xs] _ cell _ = cell ++ [xs]
    getNewBoard' (xs: s) i cell y = 
        if i == y
            then getNewBoard' s (y + 1) (cell ++ [M]) y
            else getNewBoard' s (i + 1) (cell ++ [xs]) y


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
                    cellValue = board!!x!!y
                    result = getResult cellValue
                    resultLength = length result
            Left e -> "Something went wrong"

    getResult :: CellStatus -> String
    getResult cell = if cell == N
        then "MISS"
        else "HIT"


    playGame :: String -> [[CellStatus]] -> String -> Manager -> String -> IO()
    playGame gameId opGrid history manager var = do
        case getAMove opGrid of 
            Right nextMove -> do
                putStrLn (show nextMove)
                let strToSend = formatStringBeforePost history nextMove

                postMoves manager gameId strToSend var; -- siunciu
                receivedHistory <- (getMoves manager gameId var); -- gaunu atsakyma
                let updatedHistory = firstLast receivedHistory -- apdoroju atsakyma IR TURECIAU PARASYT, AR HIT ANAS MOVE
                let updatedUpdatedHistory = formatStringAfterGet updatedHistory--getUpdatedString myBoard updatedHistory
                case parseStringToMessage updatedUpdatedHistory of
                    Right gameMessage -> 
                        case isGameOver gameMessage of
                            Right True -> putStrLn ("Game completed Successfully")
                            Right False -> 
                                playGame gameId (getNewBoard opGrid 1 [] (stringCoordToNumbers (coord gameMessage))) updatedUpdatedHistory manager var
                            Left e -> putStrLn e
                    Left e -> putStrLn e
            Left e -> putStrLn "Something went wrong when getting a move"


    getMoves :: Manager -> String -> String -> IO(String)
    getMoves manager gameId var = do
        getRequestUrl <- parseUrl $ baseUrl ++ gameId ++ "/player/" ++ var 
        putStrLn (show getRequestUrl)
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
        putStrLn gameStr
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

