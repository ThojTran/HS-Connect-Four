-- src/core/Types.hs
module Core.Types where

-- Người chơi
data Player = Red | Yellow
  deriving (Eq, Show, Read)

-- Ô trên bàn cờ
data Cell = Empty | Filled Player
  deriving (Eq, Show)

-- Bàn cờ
newtype Board = Board [[Cell]]
  deriving (Eq, Show)

-- Trạng thái game
data GameState = GameState
  { board :: Board
  , currentPlayer :: Player
  , moveHistory :: [Int]
  } deriving (Eq, Show)

-- Kết quả game
data GameResult 
  = Winner Player
  | Draw
  | InProgress
  deriving (Eq, Show)