module SyslogTcpServer where

import Network.Socket
import Control.Concurrent
import System.IO (IOMode(..), hGetContents, hClose)
import Steshaw (forever)

host = 0 -- seems to be loopback/localhost
listenPort = 2514 -- syslog port (514) + 2000 for non-root
recvSize = 4096

mkServerSocket = do
  let protocolNumber = defaultProtocol
  let family = AF_INET
  let socketType = Stream
  socket family socketType protocolNumber

-- Basic forking TCP server. Hangs up after first recv.
serve = do
  serverSocket <- mkServerSocket
  bindSocket serverSocket (SockAddrInet listenPort host)
  listen serverSocket 100
  forever $ do
    (clientSocket, sockAddr) <- accept serverSocket
    forkIO $ do
      putStrLn $ "clientSocket: " ++ (show clientSocket)
      putStrLn $ "sockAddr: " ++ (show sockAddr)
      handle <- socketToHandle clientSocket ReadMode
      hGetContents handle >>= putStrLn
      hClose handle
