{-# LANGUAGE FlexibleInstances #-}

class Printable a where
  printMe :: a -> IO ()

instance Printable a where
  printMe a = putStrLn "general instance"

instance {-# INCOHERENT #-} Printable Int where
  printMe a = putStrLn "int instance"

main :: IO ()
main = fn (5 :: Int)

fn :: a -> IO ()
fn x = printMe x
