import Prelude hiding (getLine, putStr, putStrLn)

getLine :: IO String
getLine = do 
  c <- getChar
  if c == '\n' then 
    return []
  else do
    cs <- getLine
    return (c:cs)

putStr :: String -> IO ()
putStr []     = return ()
putStr (x:xs) = do putChar x
                   putStr xs

putStr' :: String -> IO ()
putStr' = foldr f (return ())
  where
    f :: Char -> IO () -> IO ()
    f char rest = putChar char >> rest

putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'
