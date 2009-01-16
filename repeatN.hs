module Main where

repeatN 0 a = return ()
repeatN n a = a >> repeatN (n-1) a

main = repeatN 3 $ do
  putStrLn "Test"
