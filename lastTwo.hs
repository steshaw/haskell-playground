
printLastTwo :: String -> IO ()
printLastTwo = putStrLn . lastTwo

lastTwo :: String -> String
lastTwo s = l1 : l0 : []
  where
   l0 = last s
   init0 = init s
   l1 = last init0
