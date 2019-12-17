import Control.DeepSeq (deepseq)
import Control.Exception (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.Environment (getArgs)

a :: Char
a = error "asdf" `seq` 'a'

b :: IO Char
b = (fail "asdf" :: IO ()) `seq` return 'b'

c :: IO ()
c = x `seq` putStrLn "hi"
  where x = throwIO (userError "boo")

d :: MonadIO m => m ()
d = x `seq` liftIO (putStrLn "hi")
  where x = throwIO (userError "boo")

boom s = throwIO (userError s)

boom1 :: String -> IO a
boom1 = fail

barf = boom "barf"

e :: IO ()
e = barf `seq` putStrLn "hi"

f :: IO Char
f = boom "asdf" `seq` return 'b'

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["a"] -> print a
    ["b"] -> print =<< b
    ["c"] -> print =<< c
    ["d"] -> print =<< d
    ["e"] -> print =<< e
    ["f"] -> print =<< f
    [] -> putStrLn "No arguments!"
    args -> putStrLn $ "Invalid arguments: " <> show args
