-- src/Config.hs
-- Tập trung các hằng số cấu hình
module Config where

-- ===== GAME CONFIG =====
boardRows :: Int
boardRows = 6

boardCols :: Int
boardCols = 7

winLength :: Int
winLength = 4

-- ===== NETWORK CONFIG =====
serverHost :: String
serverHost = "127.0.0.1"

serverPort :: String
serverPort = "3000"

-- Timeout cho recv (microseconds)
recvTimeout :: Int
recvTimeout = 300000000  -- 5 phút

-- Buffer size
bufferSize :: Int
bufferSize = 4096

-- Max clients
maxClients :: Int
maxClients = 10

-- ===== MESSAGE DELIMITER =====
messageDelimiter :: Char
messageDelimiter = '\n'