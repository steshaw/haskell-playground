--
-- A automated solver for "scrambled equations" - a homework assignment for my 6th grade nephew.
--
-- For example, given the scrambed equation:
--   + 1 3 = 2
-- the possible unscrambled equations are:
--   1 + 2 = 3
--   3 = 1 + 2
--   3 = 2 + 1
--   2 + 1 = 3
--
-- The solver can be invoked to solve this problem using either:
--
--   ScrambedEquations> solve "+ 1 3 = 2"
--     => ["1 + 2 = 3","3 = 1 + 2","3 = 2 + 1","2 + 1 = 3"]
--
--   ScrambedEquations> solveTokens [Op Add, Num 1, Num 3, Equals, Num 2]
--     => ["1 + 2 = 3","3 = 1 + 2","3 = 2 + 1","2 + 1 = 3"]
--
module ScrambledEquations where

import Data.List (delete, nub, permutations)
import Control.Monad

data Op = Add | Sub | Div | Mul
  deriving (Eq, Show)

data Token
  = Num Integer
  | Op Op
  | Equals
  deriving (Eq, Show)

type Parse s a = s -> Maybe (a, s)

(|||) :: Parse s a -> Parse s a -> Parse s a
p1 ||| p2 = \ts -> case p1 ts of
  Nothing -> p2 ts
  x -> x

stringParse :: Parse String [Token]
stringParse [] = Just ([], [])
stringParse s = stringParseToken s >>= \(a,s) -> 
  stringParse s >>= \(a2, s2) -> Just (a:a2, s2)

stringParseToken s =
  stringParseOp ||| stringParseNum $ skipws s

skipws :: String -> String
skipws = dropWhile (== ' ')

stringParseNum :: Parse String Token
stringParseNum s = 
  case reads s of
    [(n, s)] -> Just (Num n, skipws s)
    otherwise -> Nothing

stringParseOp :: Parse String Token
stringParseOp (c:s) =
  case c of
    '+' -> Just (Op Add, skipws s)
    '-' -> Just (Op Sub, skipws s)
    '*' -> Just (Op Mul, skipws s)
    '/' -> Just (Op Div, skipws s)
    '=' -> Just (Equals, skipws s)
    otherwise -> Nothing
stringParseOp _ = Nothing

type Equation = [Token]

data Expr
  = ENum Integer
  | EEquals Expr Expr
  | EOp Op Expr Expr
  deriving (Eq, Show)

pprint :: Expr -> String
pprint (ENum n) = show n
pprint (EEquals e1 e2) = pprint e1 ++ " = " ++ pprint e2
pprint (EOp op e1 e2) = pprint e1 ++ " " ++ op2str op ++ " " ++ pprint e2

op2str Add = "+"
op2str Sub = "-"
op2str Div = "/"
op2str Mul = "*"

eval :: Expr -> (Integer, Integer, Bool)
eval (EEquals e1 e2) = 
  let 
    r1 = evalExpr e1
    r2 = evalExpr e2 
  in (r1, r2, r1 == r2)

evalExpr (ENum n) = n
evalExpr (EOp Add e1 e2) = (evalExpr e1) + (evalExpr e2)
evalExpr (EOp Sub e1 e2) = (evalExpr e1) - (evalExpr e2)
evalExpr (EOp Mul e1 e2) = (evalExpr e1) * (evalExpr e2)
evalExpr (EOp Div e1 e2) = (evalExpr e1) `div` (evalExpr e2)

type ParseExpr a = Parse [Token] a

-- expr1 = expr2
parse :: ParseExpr Expr
parse ts =
  parseExpr ts >>= \(e1, ts) ->
    parseEquals ts >>= \(_, ts) ->
      parseExpr ts >>= \(e2, ts) ->
        Just (EEquals e1 e2, ts)

-- FIXME: Expressions right associate. They need to left associate :(.
-- FIXME: e.g. (10 / 20) / 5 != 10 / (20 / 5)
-- FIXME: Similarly for subtraction.

parseExpr :: ParseExpr Expr
parseExpr = parseExprOp1 ||| parseExprL2

parseExprOp1 :: ParseExpr Expr
parseExprOp1 ts =
  parseExprL2 ts >>= \(e1, ts) ->
    parseOp1 ts >>= \(op1, ts) ->
      parseExpr ts >>= \(e2, ts) ->
        Just (EOp op1 e1 e2, ts)

parseExprL2 = parseExprOp2

-- expr mulOp tail
parseExprOp2 :: ParseExpr Expr
parseExprOp2 ts =
  parseExprL3 ts >>= \(e1, ts) ->
    parseExprOp2Tail e1 ts

-- 2 * 3 * 4 => (2 * 3) * 4
parseExprOp2Tail :: Expr -> ParseExpr Expr
parseExprOp2Tail left =
  (fooTail left) ||| (fooEmpty left)

fooTail :: Expr -> ParseExpr Expr
fooTail left ts =
  parseOp2 ts >>= \(op2, ts) ->
    parseExprL3 ts >>= \(e2, ts) ->
      parseExprOp2Tail (EOp op2 left e2) ts

fooEmpty :: Expr -> ParseExpr Expr
fooEmpty left ts = Just (left, ts)

parseExprL3 = parseNum

parseEquals :: ParseExpr Token
parseEquals (Equals:ts) = Just (Equals, ts)
parseEquals _ = Nothing

-- Operators with precedence 1
parseOp1 :: ParseExpr Op
parseOp1 (Op Add:ts) = Just (Add, ts)
parseOp1 (Op Sub:ts) = Just (Sub, ts)
parseOp1 _ = Nothing

-- Operators with precedence 2
parseOp2 :: ParseExpr Op
parseOp2 (Op Mul:ts) = Just (Mul, ts)
parseOp2 (Op Div:ts) = Just (Div, ts)
parseOp2 _ = Nothing

parseNum :: ParseExpr Expr
parseNum (Num n:ts) = Just (ENum n, ts)
parseNum otherwise = Nothing

equationToExpr :: [Token] -> Maybe Expr
equationToExpr ts = parse ts >>= \r ->
  case r of
    (e, [])   -> Just e
    otherwise -> Nothing

uniquePermutations :: Eq a => [a] -> [[a]]
uniquePermutations = nub . permutations

-- TODO: Avoid brute force.
goodPermutions :: Equation -> [Maybe Expr]
goodPermutions e = map equationToExpr (uniquePermutations e)

goodExprs :: Equation -> [Expr]
goodExprs e = map grabExpr $ filter (not . (== Nothing)) (goodPermutions e)
  where
    grabExpr (Just e) = e

solveTokens :: Equation -> [String]
solveTokens ts = map (\(e, (a,_,_)) -> pprint e) good
  where
    exprs = goodExprs ts
    results = map eval exprs
    good = filter (\(e, (_,_,b)) -> b) (zip exprs results)

solve s = grabMaybe (stringParse s >>= \(tokens, "") -> Just $ solveTokens tokens)
  where
    grabMaybe (Just a) = a
    grabMaybe Nothing = []

equation1 = [Num 3, Num 27, Equals, Num 24, Op Add]
equationString1 = "3 27 = 24 +"
prob1a = solveTokens equation1
prob1b = solve equationString1
-- 24 + 3 = 27, 3 + 24 = 27, 27 = 3 + 24, 27 = 24 + 3

equation2 = [Num 13, Op Sub, Num 15, Num 28, Equals]
equationString2 = "13 - 15 28 ="
prob2a = solveTokens equation2
prob2b = solve equationString2
-- 28 - 15 = 13, 28 - 13 = 15, 13 = 28 - 15, ...

equation3 = [Num 3, Op Sub, Num 7, Equals, Op Mul, Num 5, Num 4]
equationString3 = "3 - 7 = * 5 4"
prob3a = solveTokens equation3
prob3b = solve equationString3
-- 3 x 4 - 7 = 5, 4 x 3 - 7 = 5, 4 x 3 - 5 = 7, 3 x 4 - 5 = 7

equation4 = [Equals, Num 5, Op Sub, Num 9, Num 31, Op Mul, Num 4]
equationString4 = "= 5 - 9 31 * 4"
prob4a = solveTokens equation4
prob4b = solve equationString4
-- 4 x 9 - 5 = 31, 4 x 9 - 31 = 5, 9 x 4 - 5 = 31, 9 x 4 - 31 = 5

equation5 = [Op Mul, Op Add, Equals, Num 25, Num 11, Num 3, Num 2]
equationString5 = "* + = 25 11 3 2"
prob5a = solveTokens equation5
prob5b = solve equationString5
-- 11 x 2 + 3 = 25, 11 x 3 + 2 = 25, 2 x 11 + 3 = 25, 25 = ..., ...

probs = [(prob1a, prob1b), (prob2a, prob2b), (prob3a, prob3b), (prob4a, prob4b), (prob5a, prob5b)]

printProbs = mapM_ (\(a,b) -> do putStrLn $ show a; putStrLn $ show b; putStrLn "") $ probs

-- Expression evaluation tests.

test actual expect = 
  if expect /= actual 
  then Left $ "FAIL:- expect: " ++ show expect ++ " actual: " ++ show actual 
  else Right True

e s = stringParse s >>= \(a, _) -> parseExpr a >>= \(a, _) -> Just . evalExpr $ a

stringEval eqStr = grab (stringParse eqStr >>= \(ts, "") -> parseExpr ts >>= \(e, []) -> Just (evalExpr e))
  where
    grab (Just a) = a

stringEvalEq eqStr eq = grab (stringParse eqStr >>= \(ts, "") -> Just (ts == eq))
  where 
    grab (Just b) = b

testProbs = map (\(actual, expect) -> test actual expect) probs

tests =
  [test (stringEvalEq equationString1 equation1) True
  ,test (stringEvalEq equationString2 equation2) True
  ,test (stringEvalEq equationString3 equation3) True
  ,test (stringEvalEq equationString4 equation4) True
  ,test (stringEvalEq equationString5 equation5) True
  ,test (e "1") (Just 1)
  ,test (e "") Nothing
  ,test (e "+") Nothing
  ,test (e "=") Nothing
  ,test (e "1+1") (Just 2)
  ,test (e "1 + 1 ") (Just 2)
  ,test (e "1 + 2") (Just 3)
  ,test (e "2 + 2 / 2") (Just 3)
  ,test (e "2 / 2 + 2") (Just 3)
  ,test (stringEval "1-2-3") (-4) -- fails
  ,test (stringEval "6/3/2") 1 -- fails
  ,test (length (solve "1=1+2")) 2
  ,test (length (solve "1 1 1 = *")) 2
  ] ++ testProbs

executeTests = mapM_ (putStrLn . show) $ filter failed tests
  where
    failed (Left _) = True
    failed (Right _) = False
