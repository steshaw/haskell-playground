module SyslogTcpClient where

import Syslog
import Network.Socket
import Data.List (genericDrop)
import Data.Bits
import Control.Monad (forM_)
import Control.Concurrent
import Control.OldException
import Steshaw (forever)
import System.IO (hFlush, stdout)

data SyslogHandle = SyslogHandle
  {slhSocket :: Socket
  ,slhProgram :: String
  } deriving (Show)

openlog :: HostName
        -> String
        -> String
        -> IO SyslogHandle
openlog hostName port progName = do
  addrInfos <- getAddrInfo Nothing (Just hostName) (Just port)
  let serverAddr = head addrInfos
  sock <- socket (addrFamily serverAddr) Stream defaultProtocol
  connect sock (addrAddress serverAddr)
  return $ SyslogHandle sock progName

closelog :: SyslogHandle -> IO ()
closelog syslogH = sClose (slhSocket syslogH)

syslog :: SyslogHandle -> Facility -> Priority -> String -> IO ()
syslog syslogH fac pri msg = sendStr sendMsg
  where
    code = makeCode fac pri

    sendMsg = "<" ++ show code ++ ">" ++ (slhProgram syslogH) ++ ": " ++ msg ++ "\n"

    sendStr :: String -> IO ()
    sendStr [] = return ()
    sendStr omsg = do
      numSent <- send (slhSocket syslogH) omsg
      sendStr (genericDrop numSent omsg)

makeCode :: Facility -> Priority -> Int
makeCode fac pri = facCode `shiftL` 3 .|. priCode
  where
    facCode = codeOfFac fac
    priCode = fromEnum pri

egMessages =
  [(DEBUG, "debug")
  ,(INFO, "info")
  ,(NOTICE, "notice")
  ,(WARNING, "warning")
  ,(ERROR, "error")
  ,(CRITICAL, "critical")
  ,(ALERT, "alert")
  ,(EMERGENCY, "emergency")
  ]

eg1 = do
  log <- openlog "localhost" "2514" "eg1"
  forM_ egMessages $ \(pri, msg) -> syslog log USER pri msg
  closelog log

delayMilliSeconds n = threadDelay (n * 1000)

loopEg1 name = forever $ do
  putStrLn $ name ++ ": ."
  hFlush stdout
  handle (\e -> putStrLn $ "error: " ++ (show e)) eg1
  delayMilliSeconds 100
