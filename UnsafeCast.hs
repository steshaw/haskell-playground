module Cast where

import Data.IORef
import System.IO.Unsafe

castRef :: IORef c
castRef = unsafePerformIO $ newIORef undefined

unsafeCast :: a -> b
unsafeCast x = unsafePerformIO $ do
  writeIORef castRef x
  readIORef castRef
