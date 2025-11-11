module Network.Message where

import Core.Types
import Core.Player
import qualified Data.ByteString.Char8 as BS
import Text.Read (readMaybe)
-- Server Messages
data ServerMessage
  = SMWelcome Player              -- Chào mừng + gán màu
  | SMWaitingOpponent             -- Đợi người chơi khác
  | SMGameStart                   -- Bắt đầu game
  | SMYourTurn                    -- Đến lượt bạn
  | SMOpponentTurn                -- Đến lượt đối thủ
  | SMValidMove Int               -- Nước đi hợp lệ
  | SMInvalidMove String          -- Nước đi không hợp lệ
  | SMOpponentMove Int            -- Đối thủ đánh
  | SMGameOver GameResult         -- Kết thúc game
  | SMError String
  | SMText String                 -- Lỗi
  | SMUpdateBoard String          -- Cập nhật bảng trò chơi
  deriving (Eq, Show)

-- Client Messages
data ClientMessage
  = CMMove Int                    -- Đánh vào cột
  | CMQuit 
  | CMHelp                        -- Thoát
  deriving (Eq, Show, Read)

-- Serialize ServerMessage -> ByteString + \n
serializeSM :: ServerMessage -> BS.ByteString
serializeSM msg = BS.pack (toStr msg ++ "\n")
  where
    toStr (SMWelcome p) = "WELCOME|" ++ [playerSymbol p]
    toStr SMWaitingOpponent = "WAITING"
    toStr SMGameStart = "START"
    toStr SMYourTurn = "YOUR_TURN"
    toStr SMOpponentTurn = "OPPONENT_TURN"
    toStr (SMValidMove col) = "VALID|" ++ show col
    toStr (SMInvalidMove reason) = "INVALID|" ++ reason
    toStr (SMOpponentMove col) = "OPP_MOVE|" ++ show col
    toStr (SMGameOver result) = "GAME_OVER|" ++ showResult result
    toStr (SMError err) = "ERROR|" ++ err
    toStr (SMUpdateBoard boardStr) = "UPDATE_BOARD|" ++ boardStr -- * THÊM DÒNG NÀY *
    
    showResult (Winner p) = "WIN|" ++ [playerSymbol p]
    showResult Draw = "DRAW"
    showResult InProgress = "PROGRESS"

-- Deserialize ByteString -> ServerMessage
deserializeSM :: BS.ByteString -> Maybe ServerMessage
deserializeSM bs = parseMsg (BS.unpack $ BS.takeWhile (/= '\n') bs)
  where
    parseMsg str = case break (== '|') str of
      ("WELCOME", '|':p:[]) -> parsePlayer p >>= Just . SMWelcome
      ("WAITING", _) -> Just SMWaitingOpponent
      ("START", _) -> Just SMGameStart
      ("YOUR_TURN", _) -> Just SMYourTurn
      ("OPPONENT_TURN", _) -> Just SMOpponentTurn
      ("VALID", '|':col) -> readMaybe col >>= Just . SMValidMove
      ("INVALID", '|':reason) -> Just $ SMInvalidMove reason
      ("OPP_MOVE", '|':col) -> readMaybe col >>= Just . SMOpponentMove
      ("GAME_OVER", '|':result) -> parseResult result >>= Just . SMGameOver
      ("ERROR", '|':err) -> Just $ SMError err
      ("UPDATE_BOARD", '|':boardStr) -> Just $ SMUpdateBoard boardStr -- * THÊM DÒNG NÀY *
      _ -> Nothing
    
    parseResult str = case break (== '|') str of
      ("WIN", '|':p:[]) -> parsePlayer p >>= Just . Winner
      ("DRAW", _) -> Just Draw
      ("PROGRESS", _) -> Just InProgress
      _ -> Nothing
    
    -- readMaybe :: Read a => String -> Maybe a
    -- readMaybe s = case reads s of
    --   [(val, "")] -> Just val
    --   _ -> Nothing

-- Serialize ClientMessage -> ByteString + \n
serializeCM :: ClientMessage -> BS.ByteString
serializeCM msg = BS.pack (toStr msg ++ "\n")
  where
    toStr (CMMove col) = "MOVE|" ++ show col
    toStr CMQuit = "QUIT"

-- Deserialize ByteString -> ClientMessage
deserializeCM :: BS.ByteString -> Maybe ClientMessage
deserializeCM bs = parseMsg (BS.unpack $ BS.takeWhile (/= '\n') bs)
  where
    parseMsg str = case break (== '|') str of
      ("MOVE", '|':col) -> readMaybe col >>= Just . CMMove
      ("QUIT", _) -> Just CMQuit
      _ -> Nothing
    
    readMaybe :: Read a => String -> Maybe a
    readMaybe s = case reads s of
      [(val, "")] -> Just val
      _ -> Nothing