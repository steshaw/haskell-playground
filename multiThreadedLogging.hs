-- 
-- Code from haskell-cafe message: Bulat Ziganshin
--
--   http://www.haskell.org/pipermail/haskell-cafe/2005-December/012892.html
--

import Control.Concurrent
import Control.Monad
import System.IO
import System.IO.Unsafe

main = do h <- openBinaryFile "test" WriteMode
          for [1..100] $ \n ->
            forkIO $
              for [1..] $ \i ->
                logger h ("thread "++show n++" msg "++show i)
          getLine
          hClose h

lock = unsafePerformIO$ newMVar ()

logger h msg = withMVar lock $ const$ do
                 hPutStrLn h msg
                 putStrLn msg

for = flip mapM_
