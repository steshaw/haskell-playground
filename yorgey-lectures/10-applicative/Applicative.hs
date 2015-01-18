{-# language GeneralizedNewtypeDeriving #-}

import Control.Applicative

newtype Name  = Name String deriving (Show)
newtype Phone = Phone String deriving (Show)

data Employee = Employee
  { employeeName  :: Name
  , employeePhone :: Phone
  }
  deriving (Show)

maybeLift :: (Name -> Phone -> Employee) -> (Maybe Name -> Maybe Phone -> Maybe Employee)
maybeLift _ Nothing _ = Nothing
maybeLift _ _ Nothing = Nothing
maybeLift f (Just name) (Just phone) = Just $ f name phone

listLift :: (Name -> Phone -> Employee) -> ([Name] -> [Phone] -> [Employee])
listLift f names phones= do
  name <- names
  phone <- phones
  return $ f name phone

eLift :: (Name -> Phone -> Employee) -> (e -> Name) -> (e -> Phone) -> (e -> Employee)
eLift f eName eString = \e -> f (eName e) (eString e)

xmap0 :: Functor f => a -> f a
xmap0 = undefined
xmap  :: Functor f => (a -> b) -> (f a -> f b)
xmap = undefined
xmap2 :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
xmap2 h fa fb = h `fmap` fa <*> fb

liftA2 :: Applicative f => (a -> b -> c) -> (f a -> f b -> f c)
liftA2 h fa fb = h <$> fa <*> fb

liftA3 :: Applicative f => (a -> b -> c -> d) -> (f a -> f b -> f c -> f d)
liftA3 h fa fb fc = h <$> fa <*> fb <*> fc

liftX :: Applicative f => (a -> b -> c -> d) -> (f a -> b -> f c -> f d)
liftX h fa b fc = h <$> fa <*> pure b <*> fc

-- law: f `fmap` x === pure f <*> x

newtype Option a = Option (Maybe a) deriving (Show, Functor)

instance Applicative Option where
  pure = Option . Just
  (Option Nothing) <*> _ = Option Nothing
  _ <*> (Option Nothing) = Option Nothing
  (Option (Just f)) <*> (Option (Just x)) = Option $ Just (f x)

name1, name2 :: Option Name
name1 = Option $ Nothing
name2 = Option $ Just $ Name "Fred"

phone1, phone2 :: Option Phone
phone1 = Option $ Nothing
phone2 = Option $ Just $ Phone "555-123"

emp1, emp2, emp3, emp4 :: Option Employee
emp1 = Employee <$> name1 <*> phone1
emp2 = Employee <$> name1 <*> phone2
emp3 = Employee <$> name2 <*> phone1
emp4 = Employee <$> name2 <*> phone2

empAll :: [Option Employee]
empAll = [emp1, emp2, emp3, emp4]
