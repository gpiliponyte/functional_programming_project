module Main where

	import Available
	import Game
	import System.Environment
	
	main :: IO()
	main = do
		args <- getArgs
		case args of
			[gameId, gameVariant] -> do
				Game.startGame gameId gameVariant
			_ -> putStrLn "wrong number of arguments. Please provide game id (only)"