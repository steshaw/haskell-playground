module Iff where

iff :: Bool -> a -> a -> a
iff True x _ = x
iff False _ y = y
