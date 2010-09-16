module SyslogUdpServer where

import Network.Socket

host = 0 -- seems to be loopback/localhost
listenPort = 2514 -- syslog port (514) + 2000 for non-root
recvSize = 4096

forever f = f >> forever f

serverSocket = do
  let protocolNumber = defaultProtocol
  let family = AF_INET
  let socketType = Datagram
  socket family socketType protocolNumber

serve =
  serverSocket >>= \socket -> 
    bindSocket socket (SockAddrInet listenPort host) >>
    forever (do recv socket recvSize >>= putStrLn)
