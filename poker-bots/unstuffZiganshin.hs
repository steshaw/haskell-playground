--
-- From Bulat Ziganshin on the haskell-cafe list.
-- See http://www.haskell.org/pipermail/haskell-cafe/2006-January/013711.html
--

module Main where

import GHC.Conc
import System.IO
import System.Time
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Exception
import Foreign
import Pickle
import Endian
import Util
import ZLib
import Records
import Prelude hiding (read)

main =
    do args <- getArgs
       process (head args) 1000
       waitToFinish

{-# NOINLINE lock2 #-}
lock2 :: MVar Int
lock2 = unsafePerformIO $ newMVar 0

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

trace s = withMVar lock $ const $ putStrLn s

process _ 0 = return ()
process file n =
    do h <- openBinaryFile file ReadMode
       threadDelay (10^5)
       forkChild $ read_ h
       process file (n - 1)

read_ :: Handle -> IO ()
read_ h =
    do cmd <- read h (\_ -> return ()) -- lots of ALERTs
       -- you should not get any alerts if you pass in trace
       -- below and comment the line above. the lock synch seems
       -- to have a magical effect
       -- cmd <- read h trace
       eof <- hIsEOF h
       unless eof $ read_ h

read :: Handle -> (String -> IO ()) -> IO Command
read h trace =
    do TOD time1 _ <- getClockTime
       allocaBytes 4 $ \p1 ->
           do hGetBuf h p1 4
              (size', _) <- unpickle endian32 p1 0
              let size = fromIntegral $ size' - 4
              allocaBytes size $ \packet ->
                  do hGetBuf h packet size
                     TOD time5 _ <- getClockTime
                     cmd <- withMVar lock2 $ const $ unstuff packet 0 size
                     TOD time6 _ <- getClockTime
                     when (time6 - time1 > 3) $
                          fail $ "RED ALERT: time: " ++ show (time6 - time5)
                                   ++ "s, size: " ++ show size'
                                   ++ ", cmd: " ++ cmdDesc cmd
                     return $! cmd

unstuff :: Ptr Word8 -> Int -> Int -> IO Command
unstuff ptr ix size =
    do (kind, ix1) <- unpickle puCmdType ptr ix
       (cmd', _) <- unpickle (puCommand kind) ptr ix1
       case cmd' of
         SrvCompressedCommands sz (bytes, ix, src_sz) ->
             do let sz' = fromIntegral sz
                allocaBytes sz' $ \dest ->
                    do n <- uncompress (plusPtr bytes ix) src_sz dest sz'
                       cmd'' <- unstuff dest 4 n
                       return $! cmd''
         _ -> return cmd'
