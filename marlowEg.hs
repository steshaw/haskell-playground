--
-- An example that appears to have been written (and/or modified) by Simon Marlow.
-- See http://www.haskell.org/pipermail/haskell-cafe/2006-January/013698.html
--
-- Unfortunately, I haven't yet got it to compile with GHC 7.0.2.
--

module Main where

import System.IO
import System.Time
import System.Environment
import Control.Monad
import Control.Concurrent
import Control.Exception
import Foreign
{- XXX: The follow modules are not found
import Pickle
import Endian
import Util
import ZLib
import Records
-}
import Prelude hiding (read)
import Text.Printf

main = 
    do args <- getArgs
       process (head args) 100
       waitToFinish

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()

trace s = withMVar lock $ const $ putStrLn s

process _ 0 = return ()
process file n = 
    do h <- openBinaryFile file ReadMode
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
              TOD time2 _ <- getClockTime 
              (size', _) <- unpickle endian32 p1 0
              TOD time3 _ <- getClockTime 
              let size = fromIntegral $ size' - 4
              allocaBytes size $ \packet -> 
                  do TOD time4 _ <- getClockTime 
                     hGetBuf h packet size
                     TOD time5 _ <- getClockTime 
                     cmd <- unstuff packet 0 size
                     TOD time6 _ <- getClockTime 
                     trace $ "read: " ++ cmdDesc cmd ++ ": " 
                               ++ show (time6 - time1) ++ "s: "
                               ++ show (time2 - time1) ++ "s, "
                               ++ show (time3 - time2) ++ "s, "
                               ++ show (time4 - time3) ++ "s, "
                               ++ show (time5 - time4) ++ "s, "
                               ++ show (time6 - time5) ++ "s"
                     when (time6 - time5 > 3) $
                          fail $ "RED ALERT: time: " ++ show (time6 - time5) 
                                   ++ "s, size: " ++ show size' 
                                   ++ ", cmd: " ++ cmdDesc cmd
                     return $! cmd

psecdiff :: ClockTime -> ClockTime -> Integer
psecdiff (TOD secs1 psecs1) (TOD secs2 psecs2)
  = psecs2 - psecs1 + (secs2*10^12 - secs1*10^12)

unstuff :: Ptr Word8 -> Int -> Int -> IO Command
unstuff ptr ix size = 
    do t1@(TOD time1 _) <- getClockTime 
       (kind, ix1) <- unpickle puCmdType ptr ix
       t2@(TOD time2 _) <- getClockTime 
--       when (size > 40000) $ hPutStrLn stderr "unpickle start" 
       (cmd', _) <- unpickle (puCommand kind) ptr ix1
       t3@(TOD time3 _) <- getClockTime 
--       let d = psecdiff t1 t3
--                     (secs,psecs) = d `quotRem` (10^12)
--       hPrintf stdout "size: %5d, time: %3d.%06d\n" size secs (psecs `quot` 10^6)
       when (time3 - time1 > 3) $
            fail $ "ORANGE ALERT: " ++ show (time2 - time1) 
                     ++ "s, " ++ show (time3 - time2) ++ "s, "
                     ++ cmdDesc cmd' ++ ", ix1: " ++ show ix1
                     ++ ", size: " ++ show size 
       case cmd' of 
         SrvCompressedCommands sz (bytes, ix, src_sz) -> 
             do TOD time1 _ <- getClockTime 
                let sz' = fromIntegral sz
                allocaBytes sz' $ \dest -> 
                    do n <- uncompress (plusPtr bytes ix) src_sz dest sz'
                       TOD time2 _ <- getClockTime 
                       when (time2 - time1 > 3) $
                            fail $ "YELLOW ALERT: time: " 
                                     ++ show (time2 - time1) 
                                     ++ "s, size: " ++ show sz ++ ", array: "
                                     ++ show bytes
                       cmd'' <- unstuff dest 4 n
                       return $! cmd''
         _ -> return cmd'
