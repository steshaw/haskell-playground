{-# LANGUAGE ExistentialQuantification #-}

data T = forall x. X {a :: x, f :: x -> Int}

eg0 :: T
eg0 = X {a = "foo", f = const 0}

eg1 :: T
eg1 = X {a = "foo", f = (const 0 :: String -> Int)}

eg2 :: T -> Int
eg2 (X x y) = y x

{-
eg3 :: T -> T -> Int
eg3 (X a1 f1) (X a2 f2) = f2 a1
-}

data T1 x = X1 {a1 :: x, f1 :: x -> Int}

main = do 
  putStrLn $ show $ eg2 eg1
--  putStrLn $ show $ eg3 eg1 eg1
