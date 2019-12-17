{-# LANGUAGE BangPatterns #-}

-- This program only prints "2"
main :: IO ()
main = do
    let !x = print 1

    print 2
