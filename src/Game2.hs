module Game2 where

    import System.Environment
    import Network.HTTP.Client


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
            "A" -> playGameA gameId opponentsMap [] manager
            "B" -> putStrLn "Protec"

    playGameA :: String -> [String] -> [String] -> Manager -> IO()
    playGameA gameId [] [] manager = putStrLn gameId
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