-- src/core/Player.hs
module Core.Player where

import Core.Types

-- Đổi lượt chơi
switchPlayer :: Player -> Player
switchPlayer Red = Yellow
switchPlayer Yellow = Red

-- Lấy ký hiệu hiển thị
playerSymbol :: Player -> Char
playerSymbol Red = 'R'
playerSymbol Yellow = 'Y'

-- Lấy tên người chơi
playerName :: Player -> String
playerName Red = "Red"
playerName Yellow = "Yellow"

-- Parse người chơi từ ký tự
parsePlayer :: Char -> Maybe Player
parsePlayer 'R' = Just Red
parsePlayer 'Y' = Just Yellow
parsePlayer _ = Nothing

-- Kiểm tra ô có phải của người chơi không
cellBelongsTo :: Player -> Cell -> Bool
cellBelongsTo player (Filled p) = player == p
cellBelongsTo _ Empty = False