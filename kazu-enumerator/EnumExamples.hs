{-# LANGUAGE OverloadedStrings #-}

module EnumExamples where

import Control.Monad.IO.Class (liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
-- for OverloadedStrings
import Data.ByteString.Char8 ()
import Data.Enumerator hiding (map, filter)
import qualified Data.Enumerator.Binary as EB
--import qualified Data.Enumerator.List as EL
--import Data.Maybe
import GHC.Word (Word8)

consumer :: Iteratee BS.ByteString IO ()
consumer = do
  mw <- EB.head
  case mw of
    Nothing -> return ()
    Just w  -> do
      liftIO . putStr $ "XXX "
      liftIO . BS.putStrLn . BS.singleton $ w
      consumer

-- enumList :: Monad m => Integer -> [a] -> Enumerator a m b

listFeeder :: Enumerator BS.ByteString IO a
listFeeder = enumList 1 ["12", "34"]

eg1 :: IO ()
eg1 = run_ $ listFeeder $$ consumer

fileFeeder :: Enumerator BS.ByteString IO a
fileFeeder = EB.enumFile "FILE"

eg2 :: IO ()
eg2 = run_ $ fileFeeder $$ listFeeder $$ consumer

eg3 :: IO ()
eg3 = run_ $ fileFeeder $$ (listFeeder $$ consumer)

eg4 :: IO ()
eg4 = run_ $ (fileFeeder <==< listFeeder) $$ consumer

consumer2 :: Iteratee ByteString IO ()
consumer2 = do
  mw <- EB.head
  case mw of
    Nothing -> return ()
    Just w  -> do
      liftIO . putStr $ "YYY "
      liftIO . BS.putStrLn . BS.singleton $ w

eg5 :: IO ()
eg5 = run_ $ fileFeeder $$ listFeeder $$ (consumer2 >> consumer)

eg6 :: IO ()
eg6 = run_ $ listFeeder $$ EB.isolate 2 =$ consumer

eg7 :: IO ()
eg7 = run_ $ listFeeder $$ (EB.isolate 2 =$ consumer)

linefeed :: GHC.Word.Word8
linefeed = 10

byteLines :: Monad m => Enumeratee ByteString ByteString m b
byteLines = EB.splitWhen (== linefeed)

{-
enumHandleLines :: MonadIO m => Integer -> Handle -> Enumerator ByteString m ByteString
enumHandleLines n hdl = EB.enumHandle n hdl $= byteLines
-}
