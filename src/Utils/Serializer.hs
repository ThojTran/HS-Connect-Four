-- src/Utils/Serializer.hs
module Utils.Serializer where

import Core.Types
import Core.Player
import Config
import Text.Read (readMaybe)

-- Hiển thị bàn cờ
displayBoard :: Board -> String
displayBoard (Board rows) =
  let header = "  " ++ concatMap (\c -> show c ++ " ") [0..boardCols-1]
      separator = "  " ++ replicate (boardCols * 2 - 1) '-'
      rowStrings = map displayRow rows
  in unlines ([header, separator] ++ rowStrings)

displayRow :: [Cell] -> String
displayRow cells = "| " ++ concatMap displayCell cells

displayCell :: Cell -> String
displayCell Empty = ". "
displayCell (Filled player) = [playerSymbol player, ' ']

-- Hiển thị game state
displayGameState :: GameState -> String
displayGameState state =
  let boardStr = displayBoard (board state)
      playerStr = "Current player: " ++ playerName (currentPlayer state)
      moveStr = "Moves: " ++ show (length $ moveHistory state)
  in unlines [boardStr, playerStr, moveStr]

-- Serialize board
serializeBoard :: Board -> String
serializeBoard (Board rows) = concatMap serializeRow rows
  where
    serializeRow = concatMap serializeCell
    serializeCell Empty = "."
    serializeCell (Filled Red) = "R"
    serializeCell (Filled Yellow) = "Y"

-- Deserialize board
deserializeBoard :: String -> Maybe Board
deserializeBoard str
  | length str /= boardRows * boardCols = Nothing
  | otherwise = Just . Board $ chunksOf boardCols cells
  where
    cells = map parseCell str
    parseCell '.' = Empty
    parseCell 'R' = Filled Red
    parseCell 'Y' = Filled Yellow
    parseCell _ = Empty

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Serialize game state
serializeGameState :: GameState -> String
serializeGameState state =
  let boardData = serializeBoard (board state)
      playerData = [playerSymbol (currentPlayer state)]
      movesData = show (moveHistory state)
  in boardData ++ "|" ++ playerData ++ "|" ++ movesData

-- Deserialize game state
deserializeGameState :: String -> Maybe GameState
deserializeGameState str =
  case splitOn '|' str of
    [boardData, [playerChar], movesData] -> do
      b <- deserializeBoard boardData
      p <- parsePlayer playerChar
      moves <- readMaybe movesData
      return $ GameState b p moves
    _ -> Nothing
  where
    splitOn _ [] = []
    splitOn delim s = 
      let (chunk, rest) = break (== delim) s
      in chunk : case rest of
           [] -> []
           (_:xs) -> splitOn delim xs