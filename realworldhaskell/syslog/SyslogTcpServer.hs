module SyslogTcpServer where

import Network.Socket

host = 0 -- seems to be loopback/localhost
listenPort = 2514 -- syslog port (514) + 2000 for non-root
recvSize = 4096

forever f = f >> forever f

mkServerSocket = do
  let protocolNumber = defaultProtocol
  let family = AF_INET
  let socketType = Stream
  socket family socketType protocolNumber

-- Basic non-forking TCP server. Hangs up after first recv.
serve = do
  serverSocket <- mkServerSocket
  bindSocket serverSocket (SockAddrInet listenPort host)
  listen serverSocket 1
  forever $ do
    (clientSocket, sockAddr) <- accept serverSocket
    putStrLn $ "clientSocket: " ++ (show clientSocket)
    putStrLn $ "sockAddr: " ++ (show sockAddr)
    do recv clientSocket recvSize >>= putStrLn >> sClose clientSocket
