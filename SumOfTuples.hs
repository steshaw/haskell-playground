
prepend0 :: a -> () -> (a)
prepend0 a _ = (a)

prepend1 :: a -> (b) -> (a, b)
prepend1 a (b) = (a, b)

prepend2 :: a -> (b, c) -> (a, b, c)
prepend2 a (b, c) = (a, b, c)

prepend3 :: a -> (b, c, d) -> (a, b, c, d)
prepend3 a (b, c, d) = (a, b, c, d)

prepend4 :: a -> (b, c, d, e) -> (a, b, c, d, e)
prepend4 a (b, c, d, e) = (a, b, c, d, e)
