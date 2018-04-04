{-# language Rank2Types #-}

data DataPair a b = DataPair a b

dataFst :: DataPair a b -> a
dataFst (DataPair a _b) = a

dataSnd :: DataPair a b -> b
dataSnd (DataPair _a b) = b

dataP :: DataPair Integer Integer
dataP = DataPair 1 2

dataF :: Integer
dataF = dataFst dataP

dataS :: Integer
dataS = dataSnd dataP

-- FnPair :: (forall r. (a -> b -> r) -> r) -> FnPair a b
newtype FnPair a b = FnPair {
  runPair :: forall r. (a -> b -> r) -> r
}

fnMkP :: a -> b -> FnPair a b
fnMkP x y = FnPair $ \f -> f x y

fnFst :: FnPair a b -> a
fnFst (FnPair f) = f const
--fnFst (FnPair f) = f $ \a _b -> a

fnSnd :: FnPair a b -> b
fnSnd (FnPair f) = f (flip const)
--fnSnd (FnPair f) = f $ \_a b -> b

fnP = fnMkP 1 2
fnF = fnFst fnP
fnS = fnSnd fnP

test1 = dataF == fnF
test2 = dataS == fnS
