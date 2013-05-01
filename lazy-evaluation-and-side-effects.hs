-- This code refuses to compile since lessThanThirty and moreThanTwenty
-- have the wrong types. i.e. IO Bool instead of simply Bool.

lessThanThirty x = do {
  putStrLn $ (show x) ++ " less than 30?";
  return $ x < 30;
}

moreThanTwenty x = do {
  putStrLn $ (show x) ++ " more than 20?";
  return $ x > 20;
}

q0 = [ x | x <- [1, 25, 50, 5, 23 ], lessThanThirty x ]

q1 = [ x | x <- q0, moreThanTwenty x ]
