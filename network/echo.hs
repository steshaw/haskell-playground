module Main (main) where

import Network.Socket
import Control.Concurrent
import System.IO (IOMode(..), hGetContents, hClose)
import Steshaw (forever)

host = 0 -- seems to be loopback/localhost
listenPort = 8080
--recvSize = 4096

mkServerSocket = do
  let protocolNumber = defaultProtocol
  let family = AF_INET
  let socketType = Stream
  socket family socketType protocolNumber

server :: Socket -> IO ()
server sock =
  forever $ do
    acc <- accept sock
    forkIO (echo acc)

echo (clientSocket, sockAddr) = do
  putStrLn $ "clientSocket: " ++ (show clientSocket)
  putStrLn $ "sockAddr: " ++ (show sockAddr)
  handle <- socketToHandle clientSocket ReadMode
  hGetContents handle >>= putStrLn
  hClose handle

setup = do
  serverSocket <- mkServerSocket
  bindSocket serverSocket (SockAddrInet listenPort host)
  listen serverSocket 100
  server serverSocket

main = setup
