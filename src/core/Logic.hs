-- src/core/Logic.hs
module Core.Logic where

import Core.Types
import Core.Board
import Core.Player
import Config
import Data.List (transpose)

-- Tạo game mới - Red đi trước
newGame :: GameState
newGame = GameState
  { board = emptyBoard
  , currentPlayer = Red
  , moveHistory = []
  }

-- Thực hiện nước đi
makeMove :: GameState -> Int -> Maybe GameState
makeMove state col = do
  newBoard <- dropPiece (board state) col (currentPlayer state)
  return $ GameState
    { board = newBoard
    , currentPlayer = switchPlayer (currentPlayer state)
    , moveHistory = moveHistory state ++ [col]
    }

-- Kiểm tra kết quả
checkGameResult :: GameState -> GameResult
checkGameResult state
  | checkWinner (board state) Red = Winner Red
  | checkWinner (board state) Yellow = Winner Yellow
  | isBoardFull (board state) = Draw
  | otherwise = InProgress

-- Kiểm tra thắng
checkWinner :: Board -> Player -> Bool
checkWinner board@(Board rows) player =
  let horizontal = any (checkLine player) rows
      vertical = any (checkLine player) (transpose rows)
      diagonal = checkDiagonals board player
  in horizontal || vertical || diagonal

-- Kiểm tra dãy liên tiếp
checkLine :: Player -> [Cell] -> Bool
checkLine player cells = checkSequence player cells 0
  where
    checkSequence _ [] _ = False
    checkSequence p (c:cs) count
      | cellBelongsTo p c = 
          let newCount = count + 1
          in if newCount >= winLength 
             then True 
             else checkSequence p cs newCount
      | otherwise = checkSequence p cs 0

-- Kiểm tra đường chéo
checkDiagonals :: Board -> Player -> Bool
checkDiagonals (Board rows) player =
  let diags1 = getAllDiagonals rows
      diags2 = getAllDiagonals (map reverse rows)
  in any (checkLine player) diags1 || any (checkLine player) diags2

-- Lấy tất cả đường chéo
getAllDiagonals :: [[Cell]] -> [[Cell]]
getAllDiagonals matrix =
  let rows = length matrix
      cols = if null matrix then 0 else length (head matrix)
      topDiags = [getDiagonal matrix 0 c | c <- [0..cols-1]]
      leftDiags = [getDiagonal matrix r 0 | r <- [1..rows-1]]
  in filter (\d -> length d >= winLength) (topDiags ++ leftDiags)

-- Lấy một đường chéo
getDiagonal :: [[Cell]] -> Int -> Int -> [Cell]
getDiagonal matrix startR startC = go startR startC
  where
    rows = length matrix
    cols = if null matrix then 0 else length (head matrix)
    go r c
      | r >= rows || c >= cols = []
      | otherwise = (matrix !! r !! c) : go (r+1) (c+1)