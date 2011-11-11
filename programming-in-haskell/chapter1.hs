
prod []     = 1
prod (x:xs) = x * (prod xs)

qsort [] = []
qsort (x:xs) =
  qsort smaller ++ [x] ++ qsort larger
 where
  smaller = [ n | n <- xs , n <= x ]
  larger  = [ n | n <- xs , n >  x ]

rqsort [] = []
rqsort (x:xs) =
  rqsort larger ++ [x] ++ rqsort smaller
 where
  smaller = [ n | n <- xs , n <= x ]
  larger  = [ n | n <- xs , n >  x ]

oopsQsort [] = []
oopsQsort (x:xs) = 
  oopsQsort smaller ++ [x] ++ oopsQsort larger
 where
  smaller = [ n | n <- xs , n < x ]
  larger  = [ n | n <- xs , n > x ]
