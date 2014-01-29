-- word list is at https://gist.github.com/3799331

import Data.Set(Set, fromDistinctAscList, member)
import System.IO.Unsafe(unsafePerformIO)

import Data.Tree 
import Data.List(inits,tails)

--------------------
-- first step - generating valid next words
 
one_step :: String -> [String]
one_step = filter is_valid_word . generate_candidates

generate_candidates :: String -> [String]
generate_candidates []     
 = [] 
generate_candidates (c:cs)
 =    [ x : cs | x <- ['a'..'z'], x /= c ]       -- new letter at front
   ++ [ c : xs | xs <- generate_candidates cs ]  -- old letter at front

generate_v2 :: String -> [String]
generate_v2 w = [ before ++ d : tail after 
                | (before, after) <- all_splits_of w 
                , not (null after) 
                , d <- ['a' .. 'z']
                , d /= head after ]

all_splits_of :: [a] -> [([a], [a])] 
all_splits_of w = zip (inits w) (tails w)

--------------------
-- second step - load dictionary and add validity test

dict :: Set String
dict = fromDistinctAscList 
     $ lines 
     $ unsafePerformIO (readFile "fours.txt")

is_valid_word :: String -> Bool 
is_valid_word w = member w dict 


--------------------
-- third step - generating and traversing the state space

state_tree :: (s -> [s]) -> s -> Tree s
state_tree children_of state 
 = Node state [ state_tree children_of s | s <- children_of state ]

prune :: Int -> Tree a -> Tree a 
prune d (Node x cs) 
 | d <= 0    = Node x []
 | otherwise = Node x [ prune (d - 1) c | c <- cs ]

bft :: Tree a -> [a]
bft t = bft_ [t] 
bft_ :: [Tree a] -> [a]
bft_ trees = [ x | Node x _ <- trees ] ++ bft_ (concat $ map subForest trees)

--------------------
-- step four: doing a simple search

breadth_first_search :: (s -> Bool) -- goal test
                     -> (s -> [s])  -- state generator
                     -> s           -- initial state
                     -> [s]         -- solutions out
breadth_first_search is_goal children_of 
 = filter is_goal 
 . bft 
 . state_tree children_of 

word_chain a b = breadth_first_search (\w -> w == b) one_step a 

--------------------
-- step five - ensuring we get paths out

type Path = [String]

goal_path :: String -> Path -> Bool 
goal_path target ws = target == head ws 

-- there's an extra optimization here, of disallowing child words which have already 
-- been seen in the current path, hence avoiding lead -> head -> lead -> ... 
-- it doesn't cause BFS to fall over, but is fatal for DFS! 
-- see discussion at end 
child_paths :: Path -> [Path]
child_paths (w:ws) 
 = [ s : w : ws | s <- one_step w, s `notElem` ws ] 

init_path :: String -> Path
init_path w = [w]

word_chain_path a b = breadth_first_search (goal_path b) child_paths (init_path a)


--------------------
-- step six: some quick tests

-- abbreviations for common tests
-- yes you can build X-unit assertions for stuff like this
[t1,t2] = map word_chain_path ["ruby", "lead"]
t_r_c = t1 "code" 

-- stress-test, for profiling 
main = sequence [ print $ take (min 1 n) $ word_chain_path "ruby" "code" | n <- [1..1000]] >>= print


{- -------------------
   -- last words

   "child_paths" contains an extra test to prevent looping between the same words on a path.
   I forgot about this detail when writing the article - oops! You definitely need it when 
   trying DFS or best-first. 

   Another optimization: we don't want to repeat work already done, ie if "lead" gave 
   "head" and "bead", then it's pointless having "bead" explored as a child of "head" - so
   when searching "head" we do not need to visit its child "bead" because it was already
   covered as a sibling of "head". 

   EXERCISE: try adding this. Hint: when you generate child_paths from W then you have a 
   list of the words under W. Why not pass this around with the path value? (You can 
   define a new record type for this, or use tuples. 
   See http://book.realworldhaskell.org/read/types-and-functions.html#tuple.ghci:book for
   a quick intro to tuples. 

-}
