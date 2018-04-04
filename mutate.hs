import Data.IORef

main = do
  v1 <- newIORef 10  -- create a "cell" with 10 in it
  writeIORef v1 9    -- update the cell to 9
  v2 <- readIORef v1 -- get the value of the cell
  print v2           -- print the value to stdout

{-

Compile this program like so:

$ ghc --make mutate.hs
[1 of 1] Compiling Main             ( foo.hs, foo.o )
Linking foo ...

Run the executable like this:

$ ./mutate
9

-}
