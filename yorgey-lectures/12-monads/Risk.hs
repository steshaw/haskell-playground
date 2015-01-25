{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--module Risk where

import Control.Monad.Random
import Control.Arrow (first)
import Data.List (partition, sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

{-
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)
-}

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { bfAttackers :: Army, bfDefenders :: Army }
  deriving (Show)

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield {bfAttackers = 0}) = return bf
battle bf@(Battlefield {bfDefenders = 0}) = return bf
battle (Battlefield attackers defenders) =
  do
    ad <- attackDie
    dd <- defendDie
    let results = zipWith (>) ad dd
        losses = partition (==True) results
        aLosses = length $ fst $ losses
        dLosses = length $ snd $ losses
      in battle $ Battlefield (attackers - aLosses) (defenders - dLosses)
    where
      numAttackers = min attackers 3
      numDefenders = min defenders 2
      dies = fmap revSort . sequence . flip replicate die
      revSort = sortBy (flip compare)
      attackDie = dies numAttackers
      defendDie = dies numDefenders
