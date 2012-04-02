module Person where

data Address = Address 
  { street :: String
  , state :: String
  }
  deriving (Show)

data Person = Person 
  { age :: Int
  , address :: Address 
  }
  deriving (Show)

person = Person
  { age = 9
  , address = Address
    { street = "Edward Street"
    , state = "QLD"
    }
  }

person1 = person

person2 = Person
  { age = 23
  , address = Address
    { street = "Charlotte Street"
    , state = "QLD"
    }
  }
