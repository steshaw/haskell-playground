-- https://byorgey.wordpress.com/2014/05/08/avoiding-the-axiom-of-choice-part-i/
productOfSumsIsSumOfProducts :: (i -> (a,c)) -> (i -> a, i -> c)
productOfSumsIsSumOfProducts f = (\i -> fst (f i), \i -> snd (f i))
