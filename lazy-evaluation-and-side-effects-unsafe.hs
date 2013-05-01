-- This code refuses to compile since lessThanThirty and moreThanTwenty
-- have the wrong types. i.e. IO Bool instead of simply Bool.

import System.IO.Unsafe
import Control.Monad (forM_)

lessThanThirty x = do {
  putStrLn $ (show x) ++ " less than 30?";
  return $ x < 30;
}

moreThanTwenty x = do {
  putStrLn $ (show x) ++ " more than 20?";
  return $ x > 20;
}

q0 = [ x | x <- [1, 25, 50, 5, 23 ], unsafePerformIO (lessThanThirty x) ]

q1 = [ x | x <- q0, unsafePerformIO (moreThanTwenty x) ]

result = q1 `forM_` (\n -> putStrLn $ "[" ++ (show n) ++ "]")
