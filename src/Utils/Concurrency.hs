-- src/Utils/Concurrency.hs
module Utils.Concurrency where

import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.Chan
import Control.Monad (forever, void)
import Control.Exception (try, SomeException)

-- MVar với timeout
takeMVarTimeout :: Int -> MVar a -> IO (Maybe a)
takeMVarTimeout timeout mvar = do
  result <- newEmptyMVar
  
  void $ forkIO $ do
    val <- takeMVar mvar
    putMVar result (Just val)
  
  void $ forkIO $ do
    threadDelay timeout
    tryPutMVar result Nothing
    return ()
  
  takeMVar result

-- Broadcast đến nhiều channels
broadcast :: [Chan a] -> a -> IO ()
broadcast channels msg = mapM_ (`writeChan` msg) channels

-- Worker pool
startWorkerPool :: Int -> Chan a -> (a -> IO ()) -> IO ()
startWorkerPool numWorkers jobChan worker = 
  replicateM_ numWorkers $ forkIO $ forever $ do
    job <- readChan jobChan
    worker job

-- Safe modifyMVar
modifyMVarSafe :: MVar a -> (a -> IO a) -> IO ()
modifyMVarSafe mvar f = modifyMVar_ mvar f

-- Helper
replicateM_ :: Monad m => Int -> m a -> m ()
replicateM_ n action = sequence_ (replicate n action)