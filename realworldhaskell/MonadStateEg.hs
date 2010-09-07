module MonadStateEg where

import Control.Monad.State
import Control.Monad.Reader

type Foo = StateT String (State Integer)

a :: String -> Foo String
a s = put s >> return "hi"

b :: Integer -> Foo String
b i = lift (put i) >> return "ho"

type Bar = ReaderT Bool Foo

c :: Integer -> Bar String
c i =
  lift (lift (put i)) >>
    modify ("Muhaha " ++) >>
      return "nah"

d = runState outerStateResult 101
  where outerStateResult = runStateT readerResult "Hello"
        readerResult = runReaderT (c 99) True
