{-# LANGUAGE OverloadedStrings #-}
module Network.Client where

import Prelude hiding (read)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as BS
import Control.Exception (SomeException, try, finally, bracket)
import Control.Concurrent
import Control.Monad (forever, void)
import Data.IORef
import System.IO (hSetBuffering, BufferMode(..), stdout)
import Data.Char (toLower)
import Text.Read (readMaybe)

import Config
import Network.Message

-- Receive buffered messages
recvMessages :: Socket -> BS.ByteString -> IO (Either SomeException ([BS.ByteString], BS.ByteString))
recvMessages sock leftover = do
  eres <- try $ recv sock bufferSize
  case eres of
    Left ex -> return $ Left ex
    Right chunk ->
      if BS.null chunk
        then return $ Right ([], leftover)
        else
          let combined = BS.append leftover chunk
              parts = BS.split '\n' combined
              lastNewline = not (BS.null combined) && BS.last combined == '\n'
              (complete, newLeft) =
                if lastNewline
                  then (filter (not . BS.null) parts, "")
                  else (init parts, last parts)
           in return $ Right (complete, newLeft)

-- Send safe
trySend :: Socket -> ClientMessage -> IO ()
trySend sock msg = do
  let bs = BS.append (serializeCM msg) (BS.singleton messageDelimiter)
  _ <- try $ sendAll sock bs :: IO (Either SomeException ())
  return ()

-- Main
runClient :: IO ()
runClient = withSocketsDo $ do
  addr <- resolve serverHost serverPort
  bracket (open addr) close $ \sock -> do
    putStrLn $ "Connected to " ++ serverHost ++ ":" ++ serverPort
    hSetBuffering stdout NoBuffering

    stopFlag <- newEmptyMVar
    leftoverRef <- newIORef BS.empty

    _ <- forkIO $ finally (receiveLoop sock leftoverRef stopFlag) (putStrLn "Receiver done")
    _ <- forkIO $ finally (sendLoop sock stopFlag) (putStrLn "Sender done")

    takeMVar stopFlag
    putStrLn "Disconnected. Bye!"
  where
    resolve host port = do
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock (addrAddress addr)
      return sock

-- Receiver
receiveLoop :: Socket -> IORef BS.ByteString -> MVar () -> IO ()
receiveLoop sock buf stopFlag = do
  let loop = do
        leftover <- readIORef buf
        eres <- recvMessages sock leftover
        case eres of
          Left _ -> do
            putStrLn "Lost connection."
            tryPutMVar stopFlag ()
          Right (msgs, newLeft) -> do
            writeIORef buf newLeft
            if null msgs
              then tryPutMVar stopFlag ()
              else mapM_ handleMsg msgs >> loop
  void loop

handleMsg :: BS.ByteString -> IO ()
handleMsg raw =
  case deserializeSM raw of
    Nothing -> putStrLn $ "Bad message: " ++ BS.unpack raw
    Just msg -> case msg of
      SMWelcome p -> putStrLn $ "You are player: " ++ show p
      SMYourTurn -> putStrLn "Your turn!"
      SMOpponentTurn -> putStrLn "Opponent's turn."
      SMOpponentMove c -> putStrLn $ "Opponent move at: " ++ show c
      SMValidMove c -> putStrLn $ "Move OK: " ++ show c
      SMInvalidMove r -> putStrLn $ "Invalid move: " ++ r
      SMGameOver r -> putStrLn $ "Game over: " ++ show r
      _ -> putStrLn $ "Server: " ++ show msg

-- Sender
sendLoop :: Socket -> MVar () -> IO ()
sendLoop sock stopFlag = do
  let loop = do
        done <- not <$> isEmptyMVar stopFlag
        if done then return () else do
          line <- getLine
          case map toLower (trim line) of
            "quit" -> do
              trySend sock CMQuit
              void $ tryPutMVar stopFlag ()
            other -> case readMaybe other :: Maybe Int of
              Just n | n >= 0 && n < boardCols -> do
                trySend sock (CMMove n)
                loop
              _ -> do
                putStrLn "Invalid input."
                loop
  loop

trim :: String -> String
trim = f . f where f = reverse . dropWhile (== ' ')