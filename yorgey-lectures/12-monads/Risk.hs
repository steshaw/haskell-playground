{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--module Risk where

import Control.Monad.Random
import Control.Arrow (first)
import Control.Monad (replicateM)
import Data.List (partition, sortBy)

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

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

type R = Rand StdGen

battle :: Battlefield -> R Battlefield
battle bf@(Battlefield {bfAttackers = a}) | a < 2 = return bf
battle bf@(Battlefield {bfDefenders = 0}) = return bf
battle (Battlefield attackers defenders) =
  do
    ad <- attackDie
    dd <- defendDie
    let results = zipWith (>) ad dd
        losses = partition (==True) results
        aLosses = length $ fst $ losses
        dLosses = length $ snd $ losses
      in return $ Battlefield (attackers - aLosses) (defenders - dLosses)
    where
      numAttackers = min attackers 3
      numDefenders = min defenders 2
      dies = fmap revSort . flip replicateM die
      revSort = sortBy (flip compare)
      attackDie = dies numAttackers
      defendDie = dies numDefenders

invade :: Battlefield -> R Battlefield
invade battlefield = battle battlefield >>= f
  where
    f bf@(Battlefield {bfAttackers = a}) | a < 2 = return bf
    f bf@(Battlefield {bfDefenders = 0})         = return bf
    f bf                                         = invade bf

successProbN :: Int -> Battlefield -> R Double
successProbN n battlefield =
  invasions n >>= return . f
  where
    f bfs = numSuccess bfs `frac` n
      where
        numSuccess = length . filter totalDestruction
        frac a b = (fromIntegral a) / (fromIntegral b)
    invasions = flip replicateM $ invade battlefield
    totalDestruction bf = bfDefenders bf == 0

successProb :: Battlefield -> R Double
successProb = successProbN 1000
