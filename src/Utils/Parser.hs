-- src/Utils/Parser.hs
module Utils.Parser where

import Text.Read (readMaybe)
import Data.Char (isSpace, toLower)
import Config

-- Parse cột
parseColumn :: String -> Maybe Int
parseColumn str = do
  num <- readMaybe (trim str) :: Maybe Int
  if num >= 0 && num < boardCols
    then Just num
    else Nothing

-- Command
data Command 
  = Move Int
  | Quit
  | Help
  | Restart
  | Undo
  | Invalid String
  deriving (Eq, Show)

-- Parse command
parseCommand :: String -> Command
parseCommand input =
  let cmd = trim input
  in case map toLower cmd of
       "quit" -> Quit
       "q" -> Quit
       "help" -> Help
       "h" -> Help
       "restart" -> Restart
       "r" -> Restart
       "undo" -> Undo
       "u" -> Undo
       _ -> case parseColumn cmd of
              Just col -> Move col
              Nothing -> Invalid cmd

-- Trim whitespace
trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace

-- Validate move
isValidMove :: String -> Bool
isValidMove str = case parseColumn str of
  Just _ -> True
  Nothing -> False