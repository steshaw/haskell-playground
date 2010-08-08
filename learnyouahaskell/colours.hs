import Control.Monad

main = do
  colours <- forM [1..4] $ \n -> do
    putStrLn $ "Which colour do you associate with the number " ++ show n ++ "?"
    colour <- getLine
    return colour
  colours <- mapM (\n -> do
    putStrLn $ "Which colour do you associate with the number " ++ show n ++ "?"
    colour <- getLine
    return colour) [1..4]
  putStrLn "The colours that you associate with 1,2,3,4 are:"
  mapM_ putStrLn colours
