{-# OPTIONS_GHC -irealworldhaskell/ #-} -- FIXME: This pragma does not work.
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
import Control.Monad.State
import Maybe (fromJust)
import MaybeT

data Op = Add | Sub | Div | Mul
  deriving (Eq, Show)

data Token
  = Num Integer
  | Op Op
  | Equals
  deriving (Eq, Show)

newtype Parser s a = Parser {
  getParser :: MaybeT (State s) a
} deriving (Monad)

runParser :: Parser s a -> s -> (Maybe a, s)
runParser = runState . runMaybeT . getParser

execParser :: Parser [t] a -> [t] -> Maybe a
execParser p ts =
  case runParser p ts of
    (a, [])   -> a  -- a proper expression must be correctly parsed and be at eof (i.e. no more input)
    otherwise -> Nothing

(|||) :: Parser s a -> Parser s a -> Parser s a
p1 ||| p2 =
  Parser $ MaybeT $ State $ \s ->
    case runParser p1 s of
      (Nothing, _) -> runParser p2 s
      a            -> a

-- like {} in EBNF
-- e.g. num {mulOp num}
-- XXX: Seems a lot like a fold. Can this be generalised?
repeatParser :: (a -> Parser s a) -> a -> Parser s a
repeatParser aToParser left = 
  ((aToParser left) >>= repeatParser aToParser) ||| return left

lexer :: Parser String [Token]
lexer = repeatParser lexerTail []

execLexer :: String -> Maybe [Token]
execLexer = execParser lexer

lexerTail :: [Token] -> Parser String [Token]
lexerTail left =
  skipSpaces >> lexSingleToken >>= \t ->
    skipSpaces >> return (left ++ [t])

lexSingleToken :: Parser String Token
lexSingleToken = lexOp ||| lexNum

skipSpaces :: Parser String ()
skipSpaces = repeatParser (\_ -> parseWhen ' ' ()) ()

lexNum :: Parser String Token
lexNum = Parser $ MaybeT $ State (\s ->
  case reads s of
    [(n, s)] -> (Just (Num n), s)
    otherwise -> (Nothing, s)) -- XXX: Weird that I had to supply "s" here. Not in original version.

parseWhen :: Eq c => c -> a -> Parser [c] a
parseWhen a b = Parser $ MaybeT $ State $ \s -> case s of
  [] -> (Nothing, s)
  (x:xs) -> if (x == a) then (Just b, xs) else (Nothing, s)

lexOp :: Parser String Token
lexOp = parseWhen '+' (Op Add) |||
        parseWhen '-' (Op Sub) |||
        parseWhen '*' (Op Mul) |||
        parseWhen '/' (Op Div) |||
        parseWhen '=' Equals

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

evalOp :: Op -> (Integer -> Integer -> Integer)
evalOp Add = (+)
evalOp Sub = (-)
evalOp Mul = (*)
evalOp Div = div

evalExpr :: Expr -> Integer
evalExpr (ENum n) = n
evalExpr (EOp op e1 e2) = (evalOp op) (evalExpr e1) (evalExpr e2)

type ParseExpr a = Parser [Token] a

-- expr1 = expr2
parse :: ParseExpr Expr
parse =
  parseExpr >>= \e1 ->
    parseEquals >> parseExpr >>= \e2 ->
      return (EEquals e1 e2)

execParseExpr = execParser parseExpr

parseExpr :: ParseExpr Expr
parseExpr = parseExprL1

execParse = execParser parse

parseExprL1 :: ParseExpr Expr
parseExprL1 = parseExprL2 >>= parseExprL1Tail

parseExprL1Tail :: Expr -> ParseExpr Expr
parseExprL1Tail = repeatParser l1Tail

l1Tail :: Expr -> ParseExpr Expr
l1Tail left =
  parseOp1 >>= \op1 ->
    parseExprL2 >>= \e2 ->
      parseExprL1Tail (EOp op1 left e2)

parseExprL2 :: ParseExpr Expr
parseExprL2 = parseExprL3 >>= parseExprL2Tail

-- 2 * 3 * 4 => (2 * 3) * 4
parseExprL2Tail :: Expr -> ParseExpr Expr
parseExprL2Tail = repeatParser l2Tail

l2Tail :: Expr -> ParseExpr Expr
l2Tail left =
  parseOp2 >>= \op2 ->
    parseExprL3 >>= \e2 ->
      parseExprL2Tail (EOp op2 left e2)

parseExprL3 = parseNum

parseEquals :: ParseExpr Token
parseEquals = parseWhen Equals Equals

-- Operators with precedence 1
parseOp1 :: ParseExpr Op
parseOp1 = parseWhen (Op Add) Add |||
           parseWhen (Op Sub) Sub

-- Operators with precedence 2
parseOp2 :: ParseExpr Op
parseOp2 = parseWhen (Op Mul) Mul |||
           parseWhen (Op Div) Div

parseNum :: ParseExpr Expr
parseNum = Parser $ MaybeT $ State $ \ts -> case ts of
  (Num n:ts) -> (Just (ENum n), ts)
  otherwise  -> (Nothing, ts)

uniquePermutations :: Eq a => [a] -> [[a]]
uniquePermutations = nub . permutations

-- TODO: Avoid brute force.
goodPermutations :: Equation -> [Equation]
goodPermutations equation = uniquePermutations equation

goodExprs :: Equation -> [Expr]
-- FIXME: fromJust is kind of evil, yes?
goodExprs equation = map fromJust $ filter (not . (== Nothing)) exprs
  where
    exprs = map execParse (goodPermutations equation)

solveTokens :: Equation -> [String]
solveTokens ts = map (\(e, (a,_,_)) -> pprint e) good
  where
    exprs = goodExprs ts
    results = map eval exprs
    good = filter (\(e, (_,_,b)) -> b) (zip exprs results)

solve :: String -> [String]
-- FIXME: fromJust is kind of evil, yes?
solve s = fromJust $ execLexer s >>= return . solveTokens

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

evalString :: String -> Maybe Integer
evalString s = execLexer s >>= execParseExpr >>= return . evalExpr

testProbs :: [Either String Bool]
testProbs = map (\(actual, expect) -> test actual expect) probs

tests :: [Either String Bool]
tests =
  [test (execLexer equationString1) (Just equation1)
  ,test (execLexer equationString2) (Just equation2)
  ,test (execLexer equationString3) (Just equation3)
  ,test (execLexer equationString4) (Just equation4)
  ,test (execLexer equationString5) (Just equation5)
  ,test (evalString "1") (Just 1)
  ,test (evalString "") Nothing
  ,test (evalString "+") Nothing
  ,test (evalString "=") Nothing
  ,test (evalString "1+1") (Just 2)
  ,test (evalString "1 + 1 ") (Just 2)
  ,test (evalString "1 + 2") (Just 3)
  ,test (evalString "2 + 2 / 2") (Just 3)
  ,test (evalString "2 / 2 + 2") (Just 3)
  ,test (evalString "1-2-3") (Just (-4))
  ,test (evalString "6/3/2") (Just 1)
  ,test (length (solve "1=1+2")) 2
  ,test (length (solve "1 1 1 = *")) 2
  ] ++ testProbs

executeTests = mapM_ (putStrLn . show) $ filter failed tests
  where
    failed (Left _) = True
    failed (Right _) = False
