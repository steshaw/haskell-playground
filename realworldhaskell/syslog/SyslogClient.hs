module SyslogClient where

import Syslog
import Network.Socket
import Data.List (genericDrop)
import Data.Bits

data SyslogHandle = SyslogHandle 
  {slhSocket :: Socket
  ,slhProgram :: String
  ,slhAddress :: SockAddr
  }

openlog :: HostName
        -> String
        -> String
        -> IO SyslogHandle
openlog hostName port progName = do
  addrInfos <- getAddrInfo Nothing (Just hostName) (Just port)
  let serverAddr = head addrInfos

  sock <- socket (addrFamily serverAddr) Datagram defaultProtocol

  return $ SyslogHandle sock progName (addrAddress serverAddr)

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogH fac pri msg = sendStr sendMsg
  where
    code = makeCode fac pri
    sendMsg = "<" ++ show code ++ ">" ++ (slhProgram syslogH) ++ ": " ++ msg

    sendStr :: String -> IO ()
    sendStr [] = return ()
    sendStr omsg = do
      numSent <- sendTo (slhSocket syslogH) omsg (slhAddress syslogH)
      sendStr (genericDrop numSent omsg)

makeCode :: Facility -> Priority -> Int
makeCode fac pri = facCode `shiftL` 3 .|. priCode
  where
    facCode = codeOfFac fac
    priCode = fromEnum pri
