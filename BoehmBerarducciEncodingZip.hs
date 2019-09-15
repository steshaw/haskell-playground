--
-- Discovered at https://mail.haskell.org/pipermail/haskell-cafe/2012-September/103554.html
--
{-# LANGUAGE PolymorphicComponents #-}

module BoehmBerarducciEncodingZip where

newtype L a = L { unL :: forall r. (a -> r -> r) -> r -> r }

nil :: L a
nil = L $ \_ z -> z

cons :: a -> L a -> L a
cons x (L xs) = L $ \f -> f x . xs f

newtype A a c = Roll { unroll :: (a -> A a c -> L c) -> L c }

type B a c = a -> A a c -> L c

zipWith :: (a -> b -> c) -> L a -> L b -> L c
zipWith f (L as) (L bs) = unroll (as consA nilA) (bs consB nilB)
 where
 -- nilA :: A a c
 nilA = Roll $ const nil

 -- consA :: a -> A a c -> A a c
 consA x xs = Roll $ \k -> k x xs

 -- nilB :: B a c
 nilB _ _ = nil

 -- consB :: b -> B a c -> B a c
 consB y ys x xs = f x y `cons` unroll xs ys
