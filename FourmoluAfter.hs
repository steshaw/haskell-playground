data Record1 = Record1 {a1 :: Int64, b1 :: Int64}
data Record2 = Record2
  { a2 :: Int64
  , b2 :: Int64
  }

r1 = Record1 {a1 = 1, b1 = 2}

r2 =
  Record2
    { a2 = 1
    , b2 = 2
    }

xs = [1, 3, 3]

ys =
  [ 1
  , 3
  , 3
  ]
