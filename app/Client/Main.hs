-- app/ClientMain.hs
module Main where

import Network.Client (runClient)
import System.IO (hSetBuffering, BufferMode(..), stdout)

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering  -- In ngay lập tức
  putStrLn "=== CONNECT FOUR CLIENT ==="
  putStrLn "Dang ket noi toi 127.0.0.1:3000..."
  runClient  -- KHÔNG TRUYỀN GÌ – dùng Config.serverHost, Config.serverPort