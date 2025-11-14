-- test/GameLogicSpec.hs
module LogicGameSpec (spec) where

import Test.Hspec
import Core.Types
import Core.Board
import Core.Logic
import Core.Player

spec :: Spec
spec = do
  describe "Board Operations" $ do
    it "Create board and place the pieces" $ do
      let board = emptyBoard
      dropPiece board 0 Red `shouldSatisfy` (/= Nothing)
      isColumnFull board 0 `shouldBe` False
    
    it "full column cannot be placed" $ do
      let board = emptyBoard
      let Just b1 = dropPiece board 0 Red
      let Just b2 = dropPiece b1 0 Yellow
      let Just b3 = dropPiece b2 0 Red
      let Just b4 = dropPiece b3 0 Yellow
      let Just b5 = dropPiece b4 0 Red
      let Just b6 = dropPiece b5 0 Yellow
      dropPiece b6 0 Red `shouldBe` Nothing

  describe "Player Switch" $ do
    it "turn Red <-> Yellow" $ do
      switchPlayer Red `shouldBe` Yellow
      switchPlayer Yellow `shouldBe` Red

  describe "Game Flow" $ do
    it "New game - Red play first" $ do
      let game = newGame
      currentPlayer game `shouldBe` Red
      checkGameResult game `shouldBe` InProgress
    
    it "nice" $ do
      let game = newGame
      let Just newGame = makeMove game 0
      currentPlayer newGame `shouldBe` Yellow

  describe "Win Conditions" $ do
    it "straight win - 4 cards in a row" $ do
      let board = emptyBoard
      let Just b1 = dropPiece board 0 Red
      let Just b2 = dropPiece b1 1 Red
      let Just b3 = dropPiece b2 2 Red
      let Just b4 = dropPiece b3 3 Red
      checkWinner b4 Red `shouldBe` True
    
    it "vertical win - 4 consecutive cards" $ do
      let board = emptyBoard
      let Just b1 = dropPiece board 0 Yellow
      let Just b2 = dropPiece b1 0 Yellow
      let Just b3 = dropPiece b2 0 Yellow
      let Just b4 = dropPiece b3 0 Yellow
      checkWinner b4 Yellow `shouldBe` True
    
    it "Cross Win - 4 Cards in a Row" $ do
      let board = emptyBoard
      let Just b1 = dropPiece board 0 Red
      let Just b2 = dropPiece b1 1 Yellow
      let Just b3 = dropPiece b2 1 Red
      let Just b4 = dropPiece b3 2 Yellow
      let Just b5 = dropPiece b4 2 Yellow
      let Just b6 = dropPiece b5 2 Red
      let Just b7 = dropPiece b6 3 Yellow
      let Just b8 = dropPiece b7 3 Yellow
      let Just b9 = dropPiece b8 3 Yellow
      let Just b10 = dropPiece b9 3 Red
      checkWinner b10 Red `shouldBe` True
    
    it "can't win with only 3 pieces" $ do
      let board = emptyBoard
      let Just b1 = dropPiece board 0 Red
      let Just b2 = dropPiece b1 1 Red
      let Just b3 = dropPiece b2 2 Red
      checkWinner b3 Red `shouldBe` False
