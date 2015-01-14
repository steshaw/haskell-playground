{-# LANGUAGE MultiParamTypeClasses #-}

class Blerg a b where
  blerg :: a -> b -> Bool
