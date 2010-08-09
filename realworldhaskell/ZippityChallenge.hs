--
-- There seems to be a challenge issued on p83 of RWH.
-- It states:
--   Haskell's type system makes it an interesting challenge to write functions that take
--   variable numbers of arguments.
--
-- and the footnote:
--   Unfortunately, we do not have room to address that challenge in this book.
--
-- Well, you could argue that passing a list to a function entails a variable number of arguments...
-- or that, in Haskell, a function only ever takes one argument :). However, this was said in the
-- context of zip3, zipWith3 .. zip7, zipWith7.
--
-- Thankfully the learnyouahaskell.com seemed to cover this with the use of Control.Applicative.
--

module ZippityChallenge where

import Control.Applicative
import Steshaw((>>>))

--
-- Examples given are:
--

eg1 = zip [12, 72, 93] "zippity"
-- => [(12, 'z'), (72, '1'), (93, 'p')]

eg2 = zipWith (+) [1..3] [4..6]
-- => [5, 7, 9]

--
-- This is the alternative using Applicative:
--
altEg1 = getZipList $ (,) <$> ZipList [12, 72, 93] <*> ZipList "zippity"
-- => [(12, 'z'), (72, '1'), (93, 'p')]

altEg2 = getZipList $ (+) <$> ZipList [1..3] <*> ZipList [4..6]
-- => [5, 7, 9]

--
-- Yeah... it's a bit ugly because of all the ZipList and getZipList but we need the ZipList applicative.
-- And arguably, this isn't a variable argument function either but I think it's what was intended.
--
-- This scales up to functions with any number of arguments. Here's the equivalent to zip3/zipWith3:
--

altZip3 = getZipList $ (,,) <$> ZipList [1..3] <*> ZipList [4..9] <*> ZipList [7..]
-- => [(1,4,7),(2,5,8),(3,6,9)]

altZipWith3 = getZipList $ (\a b c -> a + b * c) <$> ZipList [1..3] <*> ZipList [4..9] <*> ZipList [7..]
-- => [29, 42, 57]

--
-- So, it seems that, although there's this nice alternative called Applicative, it's so unwieldy that
-- it's still very convenient to have zip3/zipWith3 .. zip7/zipWith7 functions predefined.
-- Be nice if there was a shorter way to specify which class instance we want. Scala allows this kind of
-- flexibility because with it's simulation of typeclasses with implicits you can control which implicits
-- are in scope at any point in your program.
--
-- There may be some nicer solution but at least we can define some shorter function names:
--

z = ZipList
gzl = getZipList

altZip3' = gzl $ (,,) <$> z [1..3] <*> z [4..9] <*> z [7..]

altZipWith3' = gzl $ (\a b c -> a + b * c) <$> z [1..3] <*> z [4..9] <*> z [7..]

--
-- ... or perhaps with getZipList at the end where it is less distracting:
--
altZip3'' = (,,) <$> z [1..3] <*> z [4..9] <*> z [7..] >>> getZipList

altZipWith3'' = (\a b c -> a + b * c) <$> z [1..3] <*> z [4..9] <*> z [7..] >>> getZipList

--
-- FIXME: Strangely, the type signatures of <$> and <*> still look alien to me :(.
--
-- TODO: Can we do better? More brevity?
--
