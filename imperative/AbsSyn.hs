-- Abstract syntax of Imp -- target of a parser.
-- Developed for use in COMP3610
-- Clem Baker-Finch

module AbsSyn where

type Name = String

data Aexp = Num Int
          | Var Name
          | Aexp :+: Aexp
          | Aexp :-: Aexp
          | Aexp :*: Aexp
          deriving Show

data Bexp = TrueLit
          | FalseLit
          | Aexp :=: Aexp
          | Aexp :<=: Aexp
          | Not Bexp
          | Bexp `And` Bexp
          | Bexp `Or` Bexp
          deriving Show

data Com  = Name := Aexp
          | Skip
          | Com :~: Com
          | If Bexp Com Com
          | While Bexp Com
          deriving Show
