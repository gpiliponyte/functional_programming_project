module Game2 where

    import Network.HTTP.Client
    import Network.HTTP.Types.Status (statusCode)
    import System.Environment
    import Data.ByteString.Internal (unpackBytes)
    import Data.ByteString.Char8 (pack, unpack)
    import qualified Data.ByteString.Lazy as L
    import qualified Data.ByteString.Char8 as BS
    import qualified Data.CaseInsensitive as CI
    import GHC.Word (Word8)

    firstMove = "l5:coordl1:A1:1ee"

    data Result = Miss | Hit | Untouched deriving (Show)

    gameMap = [[True, True, True, True, False, False, True, True, False, False],
            [False, False, False, False, False, False, False, False, False, False],
            [True, True, False, False, False, False, True, True, False, False],
            [False, False, False, False, False, False, False, False, False, False],
            [False, False, False, False, False, False, False, False, False, False],
            [False, True, True, True, False, True, False, True, False, False],
            [False, False, False, False, False, False, False, False, False, False],
            [False, False, False, False, False, False, False, False, False, False],
            [False, False, False, True, True, True, False, False, True, False],
            [False, False, False, False, True, False, False, False, False, False]]

    opponentsMap = [[Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched],
        [Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched],
        [Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched],
        [Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched],
        [Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched],
        [Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched],
        [Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched],
        [Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched],
        [Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched],
        [Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched, Untouched]]

    

    baseUrl = "http://battleship.haskell.lt/game/"

    startGame :: String -> String -> IO()
    startGame gameId gameVariant = do
        manager <- newManager defaultManagerSettings
        case gameVariant of 
            "A" -> postMoves manager gameId --playGameA gameId opponentsMap (1, 1) manager
            "B" -> putStrLn "Protec"

    postMoves :: Manager -> String -> IO()
    postMoves manager gameId = do
        postRequestUrl <- parseUrl $ baseUrl ++ gameId ++ "/player/A" 
        let request = postRequestUrl { 
            method = stringToBS $ "POST"
        , requestBody = RequestBodyLBS $ word8ToByteString $ strToWord8s $ firstMove
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

    -- playGameA :: String -> [[Result]] -> (Int, Int) -> Manager -> IO()
    -- playGameA gameId [] [] manager = putStrLn gameId
    -- playGame gameId [lastMove] playedMoves manager = do
    --     let movesToSend = playedMoves ++ [lastMove]
    --     let stringToSend = encodeMoveArrayToDictionary movesToSend
    --     postMoves manager gameId stringToSend
    --     playGameA gameId [] [] manager
    -- playGameA gameId remainingMoves playedMoves manager = do
    --     let movesToSend = playedMoves ++ (take 1 remainingMoves)
    --     let stringToSend = encodeMoveArrayToDictionary movesToSend
    --     postMoves manager gameId stringToSend
        
    --     movesString <- (getMoves manager gameId)
    
    --     let moves = decodeMessage (removeBrackets movesString)
    --     putStrLn $ show (moves)
    
    --     playGameA gameId (drop 1 remainingMoves) moves manager


        -- data Cell = Cell {
    --     coord :: (String, String),
    --     hasShip :: Bool,
    --     wasHit :: Bool
    -- } deriving (Show)

    -- -- clasiccal shapes
    -- gameMap = [[Cell {coord: ("1", "A"), hasShip: True, wasHitFalse}, Cell {coord: ("1", "B"), hasShip: True, wasHitFalse}], 
    -- [], 
    -- [], 
    -- [], 
    -- [], 
    -- [], 
    -- [], 
    -- [], 
    -- [], 
    -- []];