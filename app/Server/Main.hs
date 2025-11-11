-- app/ServerMain.hs
module Main where

import Network.Server (runServer)

main :: IO ()
main = do
  putStrLn "=== CONNECT FOUR SERVER ==="
  putStrLn "Dang khoi dong tren 127.0.0.1:3000..."
  runServer  