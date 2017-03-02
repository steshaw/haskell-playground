-- |
--
-- Experiments in style. See <https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md Johan Tibell's Haskell Style Guide>.
--
-- Also experiments with <https://www.haskell.org/haddock/doc/html/ch03s08.html Haddock markup>.
--
-- This is __important__. Where as __/this/__ is even moreso.
--
module Style where

import Prelude hiding (filter)

import Control.Monad (forM_)
import Data.ByteString (ByteString)

-- | This is a __tree__.
data Tree a
    = Branch !a
             !(Tree a)
             !(Tree a)
    | Leaf

-- | This /aren't/ really exceptions.
exceptions :: [String]
exceptions =
    ["InvalidStatusCode", "MissingContentHeader", "InternalServerError"]

-- | Just sayin' hello.
sayHello :: IO ()
sayHello = do
    name <- getLine
    putStrLn $ greeting name
  where
    greeting name = "Hello, " ++ name ++ "!"

-- |
--
-- Use can use this function like this:
--
-- > filter (> 2) [1..4]
--
-- e.g.
--
-- >>> filter (>2) [1..4]
-- [3,4]
--
-- >>> filter even [1..4]
-- [2,4]
--
-- The function 'bar' has nothing to do with this function.
--
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x = x : filter p xs
    | otherwise = filter p xs

-- |
--
-- I don't have to escape my apostrophes; great, isn't it?
--
-- This function, `bar`, is not related to `Prelude.filter`.
--
bar :: IO ()
bar =
    forM_ [1 :: Integer, 2, 3] $ \n -> do
        putStrLn "Here comes a number!"
        print n

-- | This is a reference to the "Style" module.
cFunction :: Int -> Int -> IO ()
cFunction a b = putStrLn ("first: " ++ show a ++ " second: " ++ show b)

-- | This is a reference to the "Foo" module.
alloca :: Int -> (Int -> a) -> a
alloca a f = f a

-- | This function uses a lot of λ.
foo :: IO ()
foo = alloca 10 $ \a -> alloca 20 $ \b -> cFunction a b

-- | Faking out the Socket type.
data Socket

-- |
--
-- Send a message on a socket. The socket must be in a connected
-- state. Returns the number of bytes sent. Applications are
-- responsible for ensuring that all data has been sent.
--
-- You must ensure:
--
-- * The socket is open.
--
-- * The socket is ready.
--
-- * You are ready.
--
-- * No exception happen.
--
-- * You are standing on your head.
--
--     * That's right.
--     * Too true.
--
send
    :: Socket -- ^ Connected socket
    -> ByteString -- ^ Data to send
    -> IO Int -- ^ Bytes sent
send _socket _string = error "Not implemented"
