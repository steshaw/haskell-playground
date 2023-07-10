import Data.List
import Debug.Trace

generateSequence init f = init : unfoldr f init

takeIf v p = if p v then Just (v, v) else Nothing

main = do
  let xs = generateSequence (3 :: Integer) $ \n ->
        trace "Generating element...\n" $
          (n + 1) `takeIf` (< 7)
  putStrLn "take 0"
  print $ take 0 xs
  putStrLn "take 1"
  print $ take 1 xs
  putStrLn "take 2"
  print $ take 2 xs
  putStrLn "\"toList\""
  print xs -- print forces in Haskell
  let ys = [1 :: Integer, 2, 3]
  print ys -- print forces in Haskell
