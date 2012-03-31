import Control.Concurrent
import System.IO
import Network

port = PortNumber $ fromIntegral 1234

main = withSocketsDo $
     do sock <- listenOn port
        forkIO $ do
          h <- connectTo "127.0.0.1" port 
          --threadDelay $ 2 * 1000 * 1000
          hClose h
        (h, _, _) <- accept sock
        threadDelay $ 3 * 1000 * 1000 -- ensure that close happens in connecting thread
        hPutChar h '1'
        hPutStrLn stderr "putchar 1"
        hPutStrLn stderr "flush"
        hFlush h -- two separate writes are needed
        hPutChar h '2'
        hPutStrLn stderr "putchar 2"
        hPutStrLn stderr "flush"
        hClose h -- this actually does second flush and gets SIGPIPE
        hPutStrLn stderr "OK?" -- we don't get here
