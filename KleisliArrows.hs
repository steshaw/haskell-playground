--
-- From http://www.haskell.org/haskellwiki/Arrow_tutorial
--

import Control.Arrow
--import Control.Applicative (liftA2)

plusminus, double, h2 :: Kleisli [] Int Int
plusminus = Kleisli (\x -> [x, -x])
double    = arr (* 2)
h2 = arr (* 1)
{-
h2        = liftA2 (+) plusminus double 

h2Output :: [Int]
h2Output = runKleisli h2 8
-}

main :: IO ()
main = do
   let
       prepend x = arr (x ++)
       append  x = arr (++ x)
       withId  t = returnA <+> t
       xform = (withId $ prepend "<") >>>
               (withId $ append ">") >>>
               (withId $ ((prepend "!") >>> (append "!")))
       xs = ["test", "foobar"] >>= (runKleisli xform)
   mapM_ putStrLn xs
