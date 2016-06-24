-- See http://lambda-the-ultimate.org/node/4865#comment-78189
f :: Eq a => Int -> a -> Bool
f 0 x = x == x
f n x = f (n-1) [x]

main = do n <- getLine; print (f (read n) ())
