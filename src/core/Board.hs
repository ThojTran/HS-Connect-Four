-- src/core/Board.hs
module Core.Board where

import Core.Types
import Config
import Data.List (transpose)

-- Tạo bàn cờ trống
emptyBoard :: Board
emptyBoard = Board $ replicate boardRows (replicate boardCols Empty)

-- Lấy ô tại vị trí (row, col)
getCell :: Board -> Int -> Int -> Maybe Cell
getCell (Board rows) r c
  | r < 0 || r >= boardRows || c < 0 || c >= boardCols = Nothing
  | otherwise = Just $ (rows !! r) !! c

-- Đặt quân cờ vào cột
dropPiece :: Board -> Int -> Player -> Maybe Board
dropPiece board@(Board rows) col player
  | col < 0 || col >= boardCols = Nothing
  | otherwise = 
      let column = transpose rows !! col
          rowIdx = findEmptyRow column
      in case rowIdx of
           Nothing -> Nothing
           Just r -> Just $ Board $ updateMatrix rows r col (Filled player)

-- Tìm hàng trống từ dưới lên
findEmptyRow :: [Cell] -> Maybe Int
findEmptyRow cells = 
  let indexed = zip [0..] cells
      empties = filter (\(_, cell) -> cell == Empty) indexed
  in if null empties 
     then Nothing 
     else Just . fst $ last empties

-- Cập nhật ma trận
updateMatrix :: [[Cell]] -> Int -> Int -> Cell -> [[Cell]]
updateMatrix rows r c newCell = 
  let (before, row:after) = splitAt r rows
      updatedRow = take c row ++ [newCell] ++ drop (c + 1) row
  in before ++ updatedRow : after

-- Kiểm tra cột đầy
isColumnFull :: Board -> Int -> Bool
isColumnFull (Board rows) col
  | col < 0 || col >= boardCols = True
  | otherwise = 
      let topRow = head rows
          topCell = topRow !! col
      in topCell /= Empty

-- Kiểm tra bàn cờ đầy
isBoardFull :: Board -> Bool
isBoardFull board = all (isColumnFull board) [0..boardCols-1]

-- Các cột hợp lệ
validColumns :: Board -> [Int]
validColumns board = filter (not . isColumnFull board) [0..boardCols-1]