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


    startGame :: String -> String -> IO()
    startGame gameId gameVariant = do
        manager <- newManager defaultManagerSettings
        case gameVariant of 
            "A" -> 
                playGame gameId opponentsBoard "" manager "A"
            "B" -> do
                history <- (getMoves manager gameId "B")
                let updatedHistory = firstLast history
                -- case parseStringToMessage updatedHistory of
                --     Right gameMessage -> of
                --         let updatedGameMessage = getMessageWithResult gameMessage myBoard
                --         case isGameOver updatedGameMessage of
                --             Right True -> putStrLn ("Game completed Successfully")
                --             Right False -> playGame gameId (getNewBoard opGrid 1 [] (stringCoordToNumbers (coord gameMessage))) updatedHistory manager var
                --             Left e -> putStrLn e
                --     Left e -> putStrLn e

                playGame gameId opponentsBoard updatedHistory manager "B"

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
            Right message -> ((take (length history -1 ) history) ++ "6:result3:HITe")
                -- where 
                --     coords = coord message 
            Left e -> "Something went wrong"

    getResult :: [[CellStatus]] -> (String, String) -> Result
    getResult _ _ = Hit


    playGame :: String -> [[CellStatus]] -> String -> Manager -> String -> IO()
    playGame gameId opGrid history manager var = do
        case getAMove opGrid of 
            Right nextMove -> do
                let strToSend = getStrToSend nextMove history;  -- gaunu ka siusiu
                postMoves manager gameId strToSend var; -- siunciu
                receivedHistory <- (getMoves manager gameId var); -- gaunu atsakyma
                let updatedHistory = firstLast receivedHistory -- apdoroju atsakyma IR TURECIAU PARASYT, AR HIT ANAS MOVE
                -- prideti status ar HIT
                putStrLn updatedHistory
                let updatedUpdatedHistory = getUpdatedString myBoard updatedHistory
                putStrLn updatedUpdatedHistory
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
        let request = getRequestUrl {
            method = stringToBS $ "GET"
        , requestHeaders = [(makeCaseInsensitive $ stringToBS $ "Accept", stringToBS $ "application/relaxed-bencoding+nomaps")] }
    
        response <- httpLbs request manager
        putStrLn "GET:"
        putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
        
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
        Prelude.putStrLn "POST:"
        Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)

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

