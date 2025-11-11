{-# LANGUAGE OverloadedStrings #-}
module Network.Server where

import Prelude hiding (read)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as BS
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Exception (SomeException, try, finally, bracket)
import Control.Monad (forever, void)
import Data.IORef

import Config
import Core.Types
import Core.Logic
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
              lastByteIsNewline = not (BS.null combined) && BS.last combined == '\n'
              (complete, newLeft) =
                if lastByteIsNewline
                  then (filter (not . BS.null) parts, "")
                  else (init parts, last parts)
           in return $ Right (complete, newLeft)

-- Safe send
trySend :: Socket -> ServerMessage -> IO ()
trySend sock msg = do
  let bs = BS.append (serializeSM msg) (BS.singleton messageDelimiter)
  _ <- try $ sendAll sock bs :: IO (Either SomeException ())
  return ()

-- Check if socket alive
isSocketAlive :: Socket -> IO Bool
isSocketAlive sock = do
  result <- try $ sendAll sock BS.empty :: IO (Either SomeException ())
  return $ case result of
    Left _ -> False
    Right () -> True

-- Server entry
runServer :: IO ()
runServer = withSocketsDo $ do
  addr <- resolve serverHost serverPort
  bracket (open addr) close $ \sock -> do
    putStrLn $ "Server listening on " ++ serverHost ++ ":" ++ serverPort
    clientChan <- newChan
    -- Accept loop
    void $ forkIO $ forever $ do
      (conn, peer) <- accept sock
      putStrLn $ "Accepted connection from " ++ show peer
      writeChan clientChan conn

    -- Pairing loop
    forever $ do
      sock1 <- readChan clientChan
      alive1 <- isSocketAlive sock1
      if not alive1
        then void (try (close sock1) :: IO (Either SomeException ()))
        else do
          sock2 <- readChan clientChan
          alive2 <- isSocketAlive sock2
          if not alive2
            then writeChan clientChan sock1
            else void $ forkIO (gameSession sock1 sock2)
  where
    resolve host port = do
      let hints = defaultHints {addrFlags = [AI_PASSIVE], addrSocketType = Stream}
      addrs <- getAddrInfo (Just hints) (Just host) (Just port)
      case addrs of
        [] -> error "Cannot resolve server address!"
        (addr:_) -> return addr
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      bind sock (addrAddress addr)
      listen sock maxClients
      return sock

-- Game session
gameSession :: Socket -> Socket -> IO ()
gameSession sockRed sockYellow = do
  stateVar <- newMVar newGame
  buff1 <- newIORef BS.empty
  buff2 <- newIORef BS.empty

  trySend sockRed (SMWelcome Red)
  trySend sockYellow (SMWelcome Yellow)
  trySend sockRed SMYourTurn
  trySend sockYellow SMOpponentTurn

  let clientLoop mySock myPlayer oppSock myBuff = forever $ do
        leftover <- readIORef myBuff
        eres <- recvMessages mySock leftover
        case eres of
          Left _ -> do
            putStrLn $ "Player " ++ show myPlayer ++ " disconnected."
            return ()
          Right (msgs, newLeft) -> do
            writeIORef myBuff newLeft
            mapM_ (processOne mySock myPlayer oppSock stateVar) msgs

  _ <- forkIO $ clientLoop sockRed Red sockYellow buff1
  _ <- forkIO $ clientLoop sockYellow Yellow sockRed buff2

  -- Cleanup sau 5 phút không hoạt động
  void $ forkIO $ do
    threadDelay 300000000
    mapM_ (\s -> void (try (close s) :: IO (Either SomeException ()))) [sockRed, sockYellow]
    putStrLn "Session cleaned."

-- Process one message
processOne :: Socket -> Player -> Socket -> MVar GameState -> BS.ByteString -> IO ()
processOne mySock myPlayer oppSock stateVar raw =
  case deserializeCM raw of
    Nothing -> return ()
    Just (CMMove col) -> handleMove col
    Just CMQuit -> do
      trySend oppSock (SMGameOver (Winner (if myPlayer == Red then Yellow else Red)))
      putStrLn $ "Player " ++ show myPlayer ++ " quit."
    _ -> return ()
  where
    handleMove col = do
      (outs, _) <- modifyMVar stateVar $ \st ->
        if currentPlayer st /= myPlayer
          then return (st, ([(mySock, SMInvalidMove "Not your turn")], st))
          else case makeMove st col of
            Nothing -> return (st, ([(mySock, SMInvalidMove "Invalid column")], st))
            Just ns -> do
              let result = checkGameResult ns
              let outs =
                    case result of
                      InProgress ->
                        [ (mySock, SMValidMove col),
                          (oppSock, SMOpponentMove col),
                          (mySock, SMOpponentTurn),
                          (oppSock, SMYourTurn)
                        ]
                      Winner p ->
                        [ (mySock, SMValidMove col),
                          (oppSock, SMOpponentMove col),
                          (mySock, SMGameOver (Winner p)),
                          (oppSock, SMGameOver (Winner p))
                        ]
                      Draw ->
                        [ (mySock, SMValidMove col),
                          (oppSock, SMOpponentMove col),
                          (mySock, SMGameOver Draw),
                          (oppSock, SMGameOver Draw)
                        ]
              return (ns, (outs, ns))
      mapM_ (\(s, m) -> trySend s m) outs