-- test/NetworkSpec.hs
module NetworkSpec (spec) where

import Test.Hspec
import qualified Data.ByteString.Char8 as BS
import Network.Message
import Core.Types
import Core.Player

spec :: Spec
spec = do
  describe "Message Serialization" $ do
    it "serialize ServerMessage" $ do
      serializeSM (SMWelcome Red) `shouldSatisfy` BS.isPrefixOf "WELCOME"
      serializeSM SMYourTurn `shouldSatisfy` BS.isPrefixOf "YOUR_TURN"
      serializeSM (SMValidMove 3) `shouldSatisfy` BS.isPrefixOf "VALID"
    
    it "serialize ClientMessage" $ do
      serializeCM (CMMove 2) `shouldSatisfy` BS.isPrefixOf "MOVE"
      serializeCM CMQuit `shouldSatisfy` BS.isPrefixOf "QUIT"

  describe "Message Deserialization" $ do
    it "deserialize ServerMessage" $ do
      deserializeSM (BS.pack "WELCOME|R\n") `shouldBe` Just (SMWelcome Red)
      deserializeSM (BS.pack "YOUR_TURN\n") `shouldBe` Just SMYourTurn
      deserializeSM (BS.pack "INVALID\n") `shouldBe` Nothing
    
    it "deserialize ClientMessage" $ do
      deserializeCM (BS.pack "MOVE|2\n") `shouldBe` Just (CMMove 2)
      deserializeCM (BS.pack "QUIT\n") `shouldBe` Just CMQuit
      deserializeCM (BS.pack "INVALID\n") `shouldBe` Nothing

  describe "Round-trip" $ do
    it "ServerMessage round-trip" $ do
      let msg = SMWelcome Red
      deserializeSM (serializeSM msg) `shouldBe` Just msg
    
    it "ClientMessage round-trip" $ do
      let msg = CMMove 5
      deserializeCM (serializeCM msg) `shouldBe` Just msg
