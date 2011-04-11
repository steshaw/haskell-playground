module Cast where

import Data.IORef
import System.IO.Unsafe

r :: IORef c
r = unsafePerformIO $ newIORef $ error "urk"

cast :: a -> b
cast x = unsafePerformIO $ do
           writeIORef r x
           readIORef r
