module Utils where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
