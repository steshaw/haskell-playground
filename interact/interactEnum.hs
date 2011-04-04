{-# LANGUAGE OverloadedStrings #-}
module Main where

--
-- FIXME: Work-in-progress :(
--

import Data.Char (toUpper)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as S
-- for OverloadedStrings
import Data.ByteString.Char8 ()
import Data.Enumerator hiding (map, filter)
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.Text as ET
import GHC.Word (Word8)

import System.IO (Handle, stdin)

linefeed :: GHC.Word.Word8
linefeed = 10

byteLines :: Monad m => Enumeratee ByteString ByteString m b
byteLines = EB.splitWhen (== linefeed)

enumHandleLines :: MonadIO m => Integer -> Handle -> Enumerator ByteString m ByteString
enumHandleLines n hdl = EB.enumHandle n hdl $= byteLines

consumer3 :: Iteratee ByteString IO ByteString
consumer3 = do
  mw <- EB.head
  case mw of
    Nothing -> return ""
    Just w  -> do
--      liftIO . putStr $ "YYY "
--      liftIO . BS.putStrLn . BS.singleton $ w
      return $ BS.pack [w]
      consumer3

fred = do
  bs <- run_ $ enumHandleLines 1 stdin $$ consumer3
  BS.putStrLn bs

foo = EB.enumFile "foo.txt" $= byteLines

{-
main = do
  bs <- run_ $ foo $$ EB.map S.toUpper $$ consumer3
  BS.putStrLn bs
-}

someLineWiseFunction :: String -> String
someLineWiseFunction = map toUpper

{-
printStrings :: Iteratee String IO ()
printStrings = do
  mstring <- head
  case mstring of
    Nothing -> return ()
    Just string -> do
      liftIO $ putStrLn string
      printStrings
-}
