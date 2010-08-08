
newtype NewCharList = NewCharList { getNewCharList :: [Char] } deriving (Eq, Show)

data CharList = CharList { getCharList :: [Char] } deriving (Eq, Show)

data Pair b a = Pair {getPair :: (a, b)} deriving (Eq, Ord, Show)

instance Functor (Pair c) where
  fmap f (Pair (x,y)) = Pair (f x, y)

newtype CoolBool = CoolBool { getBool :: Bool } deriving (Eq, Show, Read)

alwaysHello :: CoolBool -> String
alwaysHello (CoolBool _) = "hello"
