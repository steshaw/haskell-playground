{-# OPTIONS_GHC -fno-warn-orphans #-}

-- module Party where

import Employee
import Data.Monoid
import Data.Tree

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp : emps) (fun + empFun emp)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL emps1 fun1) (GL emps2 fun2) = GL (emps1 ++ emps2) (fun1 + fun2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ fun1) gl2@(GL _ fun2) = if fun1 > fun2 then gl1 else gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a ts) = f a (map (treeFold f) ts)

company0 :: Company
company0 =
    Node (Emp "Joe" 5)
      [ Node (Emp "John" 1) []
      , Node (Emp "Sue" 5) []
      ]

company1 :: Company
company1 =
  Node (Emp "Bob" 2)
    [ Node (Emp "Joe" 5)
      [ Node (Emp "John" 1) []
      , Node (Emp "Sue" 5) []
      ]
    , Node (Emp "Fred" 3) []
    ]

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss [] = (GL [boss] (empFun boss), GL [] 0)
nextLevel boss directReports = (bestWithBoss, bestWithoutBoss)
  where
    bestWithBoss = glCons boss $ mconcat $ map snd directReports
    bestWithoutBoss = mconcat $ map top directReports
      where
        top (gl1, gl2) = gl1 `moreFun` gl2

type Company = Tree Employee

maxFun :: Company -> GuestList
maxFun company =
  let (withCEO, withoutCEO) = treeFold nextLevel company
  in withCEO `moreFun` withoutCEO

readCompany :: IO Company
readCompany = readFile "company.txt" >>= return . read

main :: IO ()
main = do
  company <- readCompany
  print $ maxFun company
