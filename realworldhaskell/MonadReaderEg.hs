module MonadReaderEg where

import Control.Monad.Reader

myName step = do
  name <- ask
  return $ step ++ "). I am " ++ name

eg1 :: Reader String [String]
eg1 = do
  a <- myName "1"
  b <- local (const "Fred") (myName "2")
  c <- myName "3"
  return [a,b,c]

eg2 = mapM_ print $ runReader eg1 "Steve"
