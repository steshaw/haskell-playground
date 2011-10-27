--
-- From http://www.techtangents.com/the-maybe-cake-puzzle/
--

module MaybeCake where

import Data.Maybe
import Control.Applicative
import Control.Monad

data Coup = Coup {
  inspectCoup :: Maybe Chook
}

data Chook = Chook {
  layEgg :: Maybe Egg
}

data Egg = Egg
  deriving Show

data Chocolate = Chocolate
  deriving Show

data Cocoa = Cocoa
  deriving Show

data Flour = Flour
  deriving Show

data Fridge = Fridge {
  fridgeChocolate :: Maybe Chocolate,
  fridgeEgg :: Maybe Egg
}

data Pantry = Pantry {
  pantryChocolate :: Maybe Chocolate,
  pantryCocoa :: Maybe Cocoa,
  pantryFlour :: Maybe Flour
}

data Bakery = Bakery {
  bakeryCake :: Maybe Cake
}

data Cake =
    MudCake Egg Chocolate Flour
  | FlourlessCake Egg Chocolate Cocoa
  | BakeryCake
  deriving Show

-- define this
{-
I’d like a cake.

Ideally, I’d like to bake it myself, but if I don’t have the ingredients, I’ll buy one from the 
bakery (if they have one).

I can make 2 types of cake:

My favourite is a mud cake (egg, chocolate, flour).
I can also make a flourless cake (egg, chocolate, cocoa).
There might be some chocolate in the fridge or the pantry.
There might be some flour in the pantry.
There might be some cocoa in the pantry.

There might be an egg in the fridge, but I’d prefer a fresh one – maybe my chook laid one?

But I think I saw a taipan around the chicken coup yesterday – hopefully he didn’t get my chook!

So, do I get a cake and, if so, which one?
-}
bakeMeACake :: Coup -> Fridge -> Pantry -> Bakery -> Maybe Cake
bakeMeACake coup fridge pantry bakery =
  undefined
