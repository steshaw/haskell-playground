-- Copyright (c) Tim Sheard
-- Portland State University
-- Subject to conditions of distribution and use; see LICENSE.txt for details.
-- Friday May 11, 2007
-- Lambda Calculator: version 1.0


module LambdaCalculator where

import Text.ParserCombinators.Parsec  -- This for defining parsers
import qualified Text.PrettyPrint.HughesPJ as PP
import Text.PrettyPrint.HughesPJ(Doc,text,int,(<>),(<+>),($$),($+$)
                                ,render,Style(..), renderStyle,Mode(..))
import Data.List(union,(\\))
import Data.Char(digitToInt)

------------------------------------------------

data Exp
  = Lam String Exp
  | Var String
  | App Exp Exp
  | Syn String Exp


-----------------------------------------------
-- Parsing

------------ Lexical tokens

lexeme p = do{ x <- p; whiteSpace; return x  }
symbol name = lexeme (string name)
whiteSpace = many(space <|> tab <|> newline)
identifier =  try (lexeme(do { c <- lower; cs <- many(lower <|> digit <|> upper); return (c:cs)}))
natural = lexeme(number 10 digit)
parens p        = between (symbol "(") (symbol ")") p

number :: Integer -> Parser Char -> Parser Integer
number base baseDigit
    = do{ digits <- many1 baseDigit
        ; let n = foldl (\x d -> base*x + toInteger (digitToInt d)) 0 digits
        ; seq n (return n)
        }

------------- Parsers for lambda terms

pExp:: Parser Exp
pExp = appExp <|> lamExp
  where appExp = do { es <- many1 simple; return(foldl1 App es) }
        lamExp = do { symbol "\\"
                    ; v <- identifier
                    ; symbol "."
                    ; e <- pExp
                    ; return(Lam v e)}

simple:: Parser Exp
simple = var <|> try(parens pExp) <|> churchNum
  where var = do { v <- identifier; return(Var v)}
        churchNum = do { char '#'
                       ; n <- natural
                       ; let f n x = Syn ("#"++show n) x
                       ; return(f n (Lam "z" (Lam "s" (church n (Var "z") (Var "s")))))}
        church 0 z s = z
        church n z s = App s (church (n-1) z s)

parse1 x s = parse (whiteSpace >> x) "keyboard input" s

parse2 x s =
  case parse1 x s of
   Right(ans) -> ans
   Left message -> error (show message)

pe = parse2 pExp

------------------------------------------------
-- creating documents

dExp :: Exp -> Doc
dExp (Syn s e) = text s
dExp (Var s) = text s
dExp (Lam x e) = PP.hsep [(PP.hsep [text "\\",text x,text "."]),(dExp e)]
dExp apply = PP.fsep (map parenApp (args apply))

args (App x y) = args x ++ [y]
args x = [x]

parenApp (x@(App _ _)) = PP.parens (dExp x)
parenApp (x@(Lam _ _)) = PP.parens (dExp x)
parenApp x = dExp x

style = (Style PageMode 70 1.5)
showe exp = putStrLn (renderStyle style (dExp exp))

instance Show Exp where
  show x = renderStyle style (dExp x)

pp (Var s) = s
pp (Syn n e) = "(Syn "++n++" "++pp e++")"
pp (App x y) = "(App "++pp x++" "++pp y++")"
pp (Lam x e) = "(Lam "++x++" "++pp e++")"

---------------------------------------------
-- free variables

varsOf (Syn n e) = varsOf e
varsOf (Var x) = [x]
varsOf (App x y) = varsOf x `union` varsOf y
varsOf (Lam x e) = [x] `union` varsOf e

freeOf (Syn n e) = freeOf e
freeOf (Var x) = [x]
freeOf (App x y) = freeOf x `union` freeOf y
freeOf (Lam x e) = freeOf e \\ [x]

---------------------------------------------
-- substitution

sub env (Syn n e) = Syn n (sub env e)
sub env (Var x) =
  case lookup x env of
    Just e -> e
    Nothing -> Var x
sub env (App x y) = App (sub env x) (sub env y)
sub env (Lam x e) = Lam new (sub env2 e)
  where env2@((_,Var new):_) = bind env x


bind env x = if elem x free
                then (x,Var(new 0 x)):env
                else (x,Var x):env
  where free = foldr union [] (map f env)
        f (name,term) = freeOf term
        new n x = if elem next free
                     then new (n+1) x
                     else next
                   where next = x++show n

------------------------------------------------
-- beta reduction

beta (Syn n e) = Just e
beta (App (Syn _ e) x) = Just(App e x)
beta (App (Lam x e) v) = Just(sub [(x,v)] e)
beta (App x y) =
   case (beta x) of
     Just a -> Just(App a y)
     Nothing -> case (beta y) of
                  Nothing -> Nothing
                  Just b -> Just(App x b)
beta (Var x) = Nothing
beta (Lam x e) =
  case (beta e) of
    Nothing -> Nothing
    Just a -> Just(Lam x a)


deepBeta (Syn n e) = Just e
deepBeta (App (Syn _ e) x) = deepBeta (App e x)
deepBeta (App (Lam x e) v) = Just(sub [(x,v)] e)
deepBeta (App x y) =
   case (deepBeta x) of
     Just a -> deep a y
     Nothing -> case (deepBeta y) of
                  Nothing -> Nothing
                  Just b -> Just(App x b)
deepBeta (Var x) = Nothing
deepBeta (Lam x e) =
  case (deepBeta e) of
    Nothing -> Nothing
    Just a -> Just(Lam x a)

deep (Lam x e) y = Just(sub [(x,y)] e)
deep (Syn _ x) y = deep x y
deep x y = Just(App x y)

betaAll 0 x = x
betaAll n x = case deepBeta x of
               Nothing -> x
               Just e -> betaAll (n-1) e

--------------------------------------------------
-- commands

data Command
  = Execute Exp
  | Set String Exp
  | Fail String
  | BetaStep
  | Com Char  deriving Show

pCom :: Parser Command
pCom = try (do { s <- identifier; symbol "="; e <- pExp; return(Set s e)}) <|>
       try (do {e <- pExp; return(Execute e)}) <|>
       try (do { char ':'; c <- satisfy (const True); return(Com c)}) <|>
       try ( eof >> return BetaStep )

command str =
  case parse1 pCom str of
    Right(x) -> x
    Left s -> Fail (show s)

------------------------------------------------------

examples =
  [("zero", "#0")
  ,("one", "#1")
  ,("two", "#2")
  ,("add", "\\ x . \\ y . \\ z . \\ s . x (y z s) s")
  ,("mult","\\ x . \\ y . \\ z . \\ s . x z (\\ n . y n s)")
  ,("y", "\\ f . (\\ x .f (x x)) (\\ x . f (x x))")
  ,("pair", "\\x.\\y.\\k.k x y")
  ,("fst", "\\p.p(\\x.\\y.x)")
  ,("snd", "\\p.p(\\x.\\y.y)")
  ,("if", "\\b.\\then.\\else.b then else")
  ,("true", "\\t.\\f.t")
  ,("false", "\\t.\\f.f")
  ,("succ", "\\n. \\z . \\s. n (s z) s")
  ,("pred", "\\n . snd(n (pair zero zero) (\\ x . pair (succ (fst x)) (fst x)))")
  ,("fact", "\\n . snd(n (pair zero one) (\\ x . pair (succ (fst x)) (mult (succ (fst x))(snd x))))" )
  ,("ifZero","\\ x . x true (\\ y . false)")
  ,("factHelp","\\ fact . \\ n . ifZero n one (mult n (fact (pred n)))")
  ,("fact2","y factHelp")
  ]

doSub [] ans = ans
doSub ((nm,v):xs) ans =  doSub xs ((nm,sub ans v):ans)

env0 = doSub temp []
  where temp = map gen examples
        gen (nm,v) = (nm,Syn nm (pe v))

----------------------------------------------------------
-- commands

check env term action =
  do { let free = freeOf term
     ; case free \\ map fst env of
        [] -> action
        left -> putStrLn ("Unknown free variables: "++ show left) >>
                return(Just(Var "?",env))}

action (term,env) (Set s e) =
  check env e (do { let ans = sub env e
                        f (nm,v) = (nm,Syn nm v)
                  ; showe ans
                  ; return(Just(Var s,(f (s,ans)):env))})
action (term,env) (Execute e) = check env e $
  do { let ans = sub env e
     ; showe ans
     ; return(Just(ans,env))}
action (term,env) BetaStep =
  do { let ans = beta term
     ; case ans of
        Just new -> showe new >> return(Just(new,env))
        Nothing -> putStrLn "<normal-form>" >> return(Just(term,env))}
action (term,env) (Com 'q') = return Nothing
action (term,env) (Com 'b') =
  do { let new = betaAll 1000 term
     ; showe new >> return(Just(new,env))}
action (term,env) (Com 'c') =
  do { showe term; return(Just(term,env))}
action env (Fail s) = do { putStrLn ("Error:\n  "++s); return(Just env)}
action (term,env) (Com 'p') =
  do { putStrLn (pp term); return(Just(term,env))}
action (term,env) (Com 'e') = putStrLn message >> return(Just (term,env))
  where message = renderStyle style (PP.fsep (map f (reverse env)))
        f (nm,v) = text nm
action (term,env) (Com s) =   putStrLn ("Unknown command:\n  "++[s]) >> return(Just(term,env))


commandP nm (Com s) = nm==s
commandP nm _ = False

betaP BetaStep = True
betaP _ = False

execP (Execute _) = True
execP _ = False

setP (Set _ _) = True
setP _ = False

commands =
  [(setP,action,         "n = term  Install 'term' under the name `n`. It becomes the current term.\n")
  ,(execP,action,        "term      Install 'term' as the current term.\n")
  ,(betaP,action,        "<return>  Take a single beta-step on the current term.\n")
  ,(commandP 'c',action, ":c        Print the current term.\n")
  ,(commandP 'e',action, ":e        List the names installed in the current environment.\n")
  ,(commandP 'b',action, ":b        Take a large number of beta-steps without pausing.\n")
  ,(commandP 'q',action, ":q        Exit the lambda calculator.\n")
  ,(commandP 'p',action, ":p        Print the current term in internal form (for system debugging).\n")
  ,(commandP '?',comment,":?        Show this set of command descriptions.")
  ]

comments = concat (map (\(pred,action,descr)-> descr) commands)
comment env com = putStrLn comments >> return(Just env)

prolog = "\n\nThis is the Lambda Calculator. Enter lambda terms to set the\n"++
         "the current term. Step the current term by entering <return>.\n"++
         "The syntax of terms is described by the following grammar:\n\n"++
         "  T ::= <variable>\n"++
         "      | T T\n"++
         "      | \\ <variable> . T\n"++
         "      | #<integer>\n\n"++
         "Type :? to see a listing of other legal commands."

-------------------------------------------------------------------------
-- Generic Top level loop

topLevelLoop table prompt parser env =
  do { menv <- topLevel table prompt parser env
     ; case menv of
        Just env1 -> topLevelLoop table prompt parser env1
        Nothing -> return () }

topLevel table prompt parser env =
  do { putStr prompt
     ; line <- getLine
     ; let com = (parser line)
           find [] = putStrLn ("Unknown command: "++line++"\n"++show com) >> return(Just env)
           find ((pred,action,descr):ps) | pred com = action env com
           find (p:ps) = find ps
     ; find table }

----------------------------------------------------
-- main

main :: IO ()
main = putStrLn prolog >>
  topLevelLoop commands "\nprompt> " command (Var "?",env0)

work= main

