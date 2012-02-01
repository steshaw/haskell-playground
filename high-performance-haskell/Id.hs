{-# LANGUAGE RankNTypes #-}
module Id where

id :: forall a. a -> a
id x = x
