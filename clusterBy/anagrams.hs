import Data.List (sort, sortBy, groupBy)
import qualified Data.List.Key as DLK
import Data.Function (on)
import Data.Char (isAlpha)
import Data.Ord (comparing)
import Control.Arrow ((&&&), (>>>))
import qualified Data.Map as M
import qualified Data.List.Stream as DLS
import qualified Data.Stream as DS

states =
  ["Alabama", "Alaska", "Arizona", "Arkansas", "California",
    "Colorado", "Connecticut", "Delaware", "Florida",
    "Georgia", "Hawaii", "Idaho", "Indiana", "Iowa", "Kansas",
    "Kentucky", "Louisiana", "Maine", "Maryland",
    "Massachusetts", "Michigan", "Minnesota", "Mississippi",
    "Missouri", "Nebraska", "Nevada", "New Jersey",
    "New York", "New Mexico", "New Hampshire",
    "North Carolina", "North Dakota", "Ohio", "Oklahoma",
    "Oregon", "Pennsylvania", "South Carolina",
    "South Dakota", "Tennessee", "Texas", "Utah",
    "Vermont", "Virginia", "Washington", "West",
    "Wisconsin", "Wyoming"]

(|>) = flip ($)

clusterBy :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy f = M.elems . M.map reverse . M.fromListWith (++)
            . map (f &&& return)

clusterBy1 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy1 f xs = xs
  |> sortBy (\a b -> f a `compare` f b)
  |> groupBy (\a b -> f a == f b)

sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn = sortBy . comparing

groupOn :: Eq b => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)

clusterBy2 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy2 f xs = xs
  |> sortOn f
  |> groupOn f

clusterBy3 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy3 f = groupOn f . sortOn f

clusterBy4 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy4 f xs = xs
  |> DLK.sort f
  |> DLK.group f

clusterBy5 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy5 f = DLK.group f . DLK.sort f

clusterBy6 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy6 f xs = xs |> map (f &&& id) |> sortBy (comparing fst) |> groupBy ((==) `on` fst) |> map (map snd)

clusterBy7 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy7 f = map (map snd)
  . groupBy ((==) `on` fst)
  . sortBy (comparing fst)
  . map (f &&& id)

clusterBy8 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy8 f = map (f &&& id) 
  >>> sortBy (comparing fst) 
  >>> groupBy ((==) `on` fst) 
  >>> map (map snd)

clusterBy9 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy9 f = map (map snd)
  . DLK.group fst
  . DLK.sort fst
  . map (f &&& id)

clusterBy10 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy10 f =  map (f &&& id)
  >>> DLK.sort fst
  >>> DLK.group fst
  >>> map (map snd)

clusterBy11 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy11 f = DLS.map (f &&& id) 
  >>> DLS.sortBy (comparing fst) 
  >>> DLS.groupBy ((==) `on` fst) 
  >>> DLS.map (DLS.map snd)

-- Hacked sortBy from GHC.
sortBy12 :: (a -> a -> Ordering) -> [a] -> [a]
sortBy12 cmp = mergeAll . sequences
  where
    sequences (a:b:xs)
      | a `cmp` b == GT = descending b [a]  xs
      | otherwise       = ascending  b (a:) xs
    sequences xs = [xs]

    descending a as (b:bs)
      | a `cmp` b == GT = descending b (a:as) bs
    descending a as bs  = (a:as): sequences bs

    ascending a as (b:bs)
      | a `cmp` b /= GT = ascending b (\ys -> as (a:ys)) bs
    ascending a as bs   = as [a]: sequences bs

    mergeAll [x] = x
    mergeAll xs  = mergeAll (mergePairs xs)

    mergePairs (a:b:xs) = merge a b: mergePairs xs
    mergePairs xs       = xs

    merge as@(a:as') bs@(b:bs')
      | a `cmp` b == GT = b:merge as  bs'
      | otherwise       = a:merge as' bs
    merge [] bs         = bs
    merge as []         = as

sortOn12 :: Ord b => (a -> b) -> [a] -> [a]
sortOn12 = sortBy12 . comparing

clusterBy12 :: Ord b => (a -> b) -> [a] -> [[a]]
clusterBy12 f = sortOn12 f >>> groupOn f

main = mapM_ print . solve . lines =<< getContents

solve = filter ((> 1) . length) . clusterBy12 signature . ucombos

ucombos xs = [[x, y] | x <- xs, y <- xs, x < y]

signature = sort . filter isAlpha . concat -- sort letters
