import Control.Concurrent
import Network
import System.IO

-- it won't SIGPIPE, when you comment second hPrint
player h hOther otherTh = evil >> hClose h
    where evil = hPrint h 13 >> hPrint h 42

doAccept sock =
     do (h1, _, _) <- accept sock
        (h2, _, _) <- accept sock
        hSetBuffering h1 LineBuffering
        hSetBuffering h2 LineBuffering
        forkIO $ myThreadId >>= forkIO . player h2 h1 >>= player h1 h2
        threadDelay 500000
        hPutStrLn stderr "OK?"

main = withSocketsDo $ listenOn (PortNumber $ fromIntegral 3372) >>= doAccept
