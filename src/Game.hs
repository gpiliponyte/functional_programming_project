{-# LANGUAGE OverloadedStrings #-}
module Game
where

import Control.Lens
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import System.Environment
import Data.ByteString.Internal (unpackBytes)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy as L
import GHC.Word (Word8)

import Decoding
import Encoding

predefinedMoves = [
	  Move 0 0 'x'
	, Move 0 2 'x'
	, Move 1 0 'x'
	, Move 1 2 'x'
	, Move 2 1 'x']

baseUrl = "http://tictactoe.homedir.eu/game/" 
urlEnding = "/player/1"
testUrlEnding = "/player/2"

movesToPostString = encodeMoveArrayToDictionary [Move 0 0 'x'] 
  
startGame :: String -> IO()
startGame gameId = do
	manager <- newManager defaultManagerSettings
	playGame gameId predefinedMoves [] manager

playGame :: String -> [Move] -> [Move] -> Manager -> IO()
playGame gameId [] moves manager = Prelude.putStrLn "The Game has been played succesfully"
playGame gameId [lastMove] playedMoves manager = do
	let movesToSend = playedMoves ++ [lastMove]
	let stringToSend = encodeMoveArrayToDictionary movesToSend
	postMoves manager gameId stringToSend
	playGame gameId [] [] manager
playGame gameId remainingMoves playedMoves manager = do
	let movesToSend = playedMoves ++ (take 1 remainingMoves)
	let stringToSend = encodeMoveArrayToDictionary movesToSend
	postMoves manager gameId stringToSend
	
	movesString <- (getMoves manager gameId)

	let moves = decodeMessage (removeBrackets movesString)
	putStrLn $ show (moves)

	playGame gameId (drop 1 remainingMoves) moves manager

testThings :: String -> IO()
testThings gameId = do
	managerTest <- newManager defaultManagerSettings	
	postMoves managerTest gameId movesToPostString
	movesString <- getMoves managerTest gameId
	putStrLn movesString


postMoves :: Manager -> String -> String -> IO()
postMoves manager gameId movesToPost = do
  postRequestUrl <- parseUrl $ "http://tictactoe.homedir.eu/game/" ++ gameId ++ "/player/1" 
  let request = postRequestUrl { 
  	  method = "POST"
    , requestBody = RequestBodyLBS $ word8ToByteString $ strToWord8s $ movesToPost
    , requestHeaders = [("Content-Type", "application/bencode+map")] }

  response <- httpLbs request manager
  Prelude.putStrLn "POST:"
  Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)


getMoves :: Manager -> String -> IO(String)
getMoves manager gameId = do
  getRequestUrl <- parseUrl $ "http://tictactoe.homedir.eu/game/" ++ gameId ++ "/player/1" 
  let request = getRequestUrl {
  	  method = "GET"
    , requestHeaders = [("Accept", "application/bencode+map")] }

  response <- httpLbs request manager
  putStrLn "GET:"
  putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)
  
  return $ show $ responseBody response


strToWord8s :: String -> [Word8]
strToWord8s = unpackBytes . pack

word8ToByteString :: [Word8] -> L.ByteString
word8ToByteString a = L.pack a

removeBrackets :: String -> String
removeBrackets ('"' : message) = reverse (drop 1 (reverse message))
removeBrackets message = message


