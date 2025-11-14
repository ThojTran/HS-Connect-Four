module Main where

import Test.Hspec
-- Gọi các module muốn test
import qualified LogicGameSpec
import qualified NetworkSpec

main :: IO ()
main = hspec $ do
  describe "Game Logic Tests" LogicGameSpec.spec
  describe "Network Tests" NetworkSpec.spec