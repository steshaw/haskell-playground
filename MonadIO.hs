import Control.Monad.IO.Class

foo1 :: IO ()
foo1 = putStrLn "foo1"

foo1a :: IO ()
foo1a = liftIO $ putStrLn "foo1a"

foo1b :: IO ()
foo1b = liftIO $ liftIO $ putStrLn "foo1b"

foo2 :: MonadIO m => m ()
foo2 = liftIO $ putStrLn "foo2"

foo3 :: MonadIO m => m ()
foo3 = liftIO $ putStrLn "foo3"

main = sequence [foo1, foo1a, foo1b, foo2, foo3]
