--
-- Crazy self-compiling Haskell subset compiler
--
-- Haskell subset → C - 18926 characters
--
-- This compiles a small subset of Haskell to C. Features it supports:
-- 
--  * Pattern matching and guards
--  * Data declarations
--  * Select infix operators
--  * Lazy evaluation
--
-- The biggest missing features are nested variables (meaning no lambda/let/where/case), type checking, and type
-- classes. Resulting programs leak memory, and self-compilation takes about 200 megabytes on my system (the Boehm
-- garbage collector helps a lot, but only if the compiler optimizes tail recursion well).
-- 
-- To bootstrap, uncomment the first three lines (not counted in the score) and compile with GHC. The compiler
-- takes Haskell-subset code on stdin and produces C code on stdout.
--

import Prelude hiding (fmap, lookup, snd, zip);import Data.Char
import Data.List hiding (lookup, zip);data P a b = P a b;data B = B
add=(+);sub=(-);showInt=show;append[]ys=ys;append(x:xs)ys=x:append xs ys

data Program = Program [[Constructor]] [Function]
data Toplevel = TD [Constructor] | TE Equation | TO
data Constructor = Constructor String Int
data Function = Function String Int [Equation]
data Equation = Equation String [Pattern] (Maybe Expression) Expression
data Pattern = PVar String | PCon String [Pattern]
data Expression = Var String | Con String | Int String | Char String | String String | Ap Expression Expression
data Environment = Environment [P String Int] [P String VarInfo]
data VarInfo = VBox String | VArg Int | VItem VarInfo Int
main = interact (compile . parse)
constructorName (Constructor name _) = name
functionName (Function name _ _) = name
equationName (Equation name _ _ _) = name
sortToplevels [] = (P [] [])
sortToplevels (TD x : xs) = applyFst ((:) x) (sortToplevels xs)
sortToplevels (TE x : xs) = applySnd ((:) x) (sortToplevels xs)
sortToplevels (TO : xs)   = sortToplevels xs
pcons x xs = PCon "Cons" [x, xs];pnil = PCon "Nil" []
ebinary op a b = Ap (Ap (Var op) a) b;ebinaryE op a b = Ap (Ap op a) b
econs x xs = Ap (Ap (Con "Cons") x) xs
enil = Con "Nil"
listEq eq [] [] = True
listEq eq (x:xs) (y:ys) | eq x y = listEq eq xs ys
listEq _ _ _ = False
snd (P a b) = b
zip = zipWith P
lookup q (P k v : _)  | listEq (==) q k = Just v
lookup q (_     : xs)                   = lookup q xs
lookup q _                              = Nothing
compose2 f g x y = f (g x y)
applyFst f (P x y) = P (f x) y
applySnd f (P x y) = P x (f y)
fMaybe f Nothing  = Nothing
fMaybe f (Just x) = Just (f x)
cond f t False = f
cond f t True = t
condList f t [] = f
condList f t xs = t xs
countFrom n = n : countFrom (add n 1)
range l h | l > h = []
range l h         = l : range (add l 1) h
parse = makeProgram . sortToplevels . concatMap parse_p . ((:) prelude) . preprocess
parse_p (P lineno line) = maybe (parse_err lineno line) snd (parseLine line)
parse_err lineno line = error (concat ["Parse error on line ", showInt lineno, ": `", line, "`"])
preprocess = filter (not . isCommentOrEmpty . snd) . zip (countFrom 1) . map (dropWhile isBlank) . lines
isCommentOrEmpty = parserSucceeds (pro (ignore (pro (parseS "--") (parseS "import "))) parseEof)
liftA2 f a b = ap (fmap f a) b
parserSucceeds p s = maybe False (const True) (p s)
fmap f p = fMaybe (applySnd f) . p
pure x s = Just (P s x)
ap1 b (P s x) = maybe Nothing (ap2 x) (b s)
ap2 x (P s y) = Just (P s (x y))
empty = (const Nothing)
pro a b s = maybe (b s) Just (a s)
ap a b = maybe Nothing (ap1 b) . a
prc = liftA2 (:)
pra = liftA2 append
prl = liftA2 const
prr = liftA2 (const id)
many p = pro (some p) (pure [])
some p = prc p (many p)
optional p = pro (fmap Just p) (pure Nothing)
choice = foldr pro (const Nothing)
parseEof = parseEof_1
parseEof_1 [] = Just (P "" B)
parseEof_1 _  = Nothing
parsePred pred = parsePred_1 pred
parsePred_1 pred (x:xs) | pred x = Just (P xs x)
parsePred_1 _    _               = Nothing
manyParsePred = justFlipSplit
justFlipSplit pred xs = Just (P (dropWhile pred xs) (takeWhile pred xs))
someParsePred pred = prc (parsePred pred) (manyParsePred pred)
parseC = parsePred . (==)
parseS = foldr (prc . parseC) (pure [])
wrapC = wrapSpace . parseC
wrapS = wrapSpace . parseS
skipPred pred = prr (parsePred pred) (pure B)
manySkipPred pred = prr (manyParsePred pred) (pure B)
preSep p sep = many (prr sep p)
sepBy1 p sep = prc p (many (prr sep p))
sepByChar p c = pro (sepByChar1 p c) (pure [])
sepByChar1 p c = sepBy1 p (wrapSpace (parseC c))
wrapSpace p = prl (prr skipSpace p) skipSpace
ignore = fmap (const B)
isBlank c | c == ' ' || c == '\t' = True
isBlank _                         = False
isDigit1 c = c >= '1' && c <= '9'
parseBetween l r p = prl (prr (parseC l) (wrapSpace p)) (parseC r)
skipSpace = manySkipPred isBlank
chainl1 f sep p = fmap (foldl1 f) (sepBy1 p sep)
chainr1 f sep p = fmap (foldr1 f) (sepBy1 p sep)
chainl f z sep p = pro (fmap (foldl f z) (sepBy1 p sep)) (pure z)
chainr f z sep p = pro (fmap (foldr f z) (sepBy1 p sep)) (pure z)
parseNonassoc ops term = liftA2 (flip ($)) term (pro (liftA2 flip (choice ops) term) (pure id))
parseVar = prc (parsePred (orUnderscore isLower)) (many (parsePred (orUnderscore isAlphaNum)))
orUnderscore p c | p c || c == '_' = True
orUnderscore _ _ = False
parseCon = prc (parsePred isUpper) (many (parsePred (orUnderscore isAlphaNum)))
parseInt = pro (parseS "0") (prc (parsePred isDigit1) (many (parsePred isDigit)))
parseEscape q (c:x:xs) | c == '\\' = Just (P xs (c:x:[]))
parseEscape q [c]      | c == '\\' = Just (P [] [c])
parseEscape q (c:xs)   | c /= q    = Just (P xs [c])
parseEscape q _                    = Nothing
parseStringLiteral q = pra (parseS [q]) (pra (fmap concat (many (parseEscape q))) (parseS [q]))
parsePattern = chainr1 pcons (wrapC ':') (pro (liftA2 PCon parseCon (preSep parsePatternPrimary skipSpace)) parsePatternPrimary)
parsePatternPrimary = choice [fmap PVar parseVar, fmap (flip PCon []) parseCon, parseBetween '(' ')' parsePattern, parseBetween '[' ']' (fmap (foldr pcons pnil) (sepByChar parsePattern ','))]
relops f = relops_1 (ops_c f)
otherops f = f ":" (Con "Cons") : otherops_1 (ops_c f)
ops_c f x y = f x (Var y)
relops_1 f   = [f "<=" "_le", f "<" "_lt", f "==" "_eq", f ">=" "_ge", f ">" "_gt", f "/=" "_ne"]
otherops_1 f = [f "$" "_apply", f "||" "_or", f "&&" "_and", f "." "_compose"]
parseRelops = parseNonassoc (relops parseRelops_f)
parseRelops_f op func = prr (wrapS op) (pure (ebinaryE func))
parseExpression = chainr1 (ebinary "_apply") (wrapC '$') $ chainr1 (ebinary "_or") (wrapS "||") $ chainr1 (ebinary "_and") (wrapS "&&") $ parseRelops $ chainr1 econs (wrapC ':') $ chainr1 (ebinary "_compose") (wrapC '.') $ chainl1 Ap skipSpace $ choice [fmap Var parseVar, fmap Con parseCon, fmap Int parseInt, fmap Char (parseStringLiteral '\''), fmap String (parseStringLiteral '"'), parseBetween '(' ')' (pro parseSection parseExpression), parseBetween '[' ']' (chainr econs enil (wrapC ',') parseExpression)]
parseSection = choice (append (relops parseSection_f) (otherops parseSection_f))
parseSection_f op func = prr (wrapS op) (pure func)
parseEquation = ap (ap (ap (fmap Equation parseVar) (many (prr skipSpace parsePatternPrimary))) (optional (prr (wrapC '|') parseExpression))) (prr (wrapC '=') parseExpression)
skipType = ignore (sepBy1 (sepBy1 skipTypePrimary skipSpace) (wrapS "->"))
skipTypePrimary = choice [ignore parseVar, ignore parseCon, parseBetween '(' ')' skipType, parseBetween '[' ']' skipType]
parseDataDecl = prr (parseS "data") (prr skipSpace (prr parseCon (prr (preSep parseVar skipSpace) (prr (wrapC '=') (sepByChar1 (liftA2 Constructor parseCon (fmap length (preSep skipTypePrimary skipSpace))) '|')))))
skipTypeSignature = prr parseVar (prr (wrapS "::") skipType)
skipTypeAlias = prr (parseS "type") (prr skipSpace (prr parseCon (prr (preSep parseVar skipSpace) (prr (wrapC '=') skipType))))
parseToplevel = choice [fmap (const TO) (pro skipTypeSignature skipTypeAlias), fmap TD parseDataDecl, fmap TE parseEquation]
parseLine = prl (prl (sepByChar1 parseToplevel ';') skipSpace) parseEof
patternCount (Equation _ ps _ _) = length ps
makeProgram (P ds es) = Program ds (makeFunctions es)
makeFunctions = map makeFunctions_f . groupBy makeFunctions_g
makeFunctions_f []     = error "Internal error: No equations in binding group"
makeFunctions_f (x:xs) = cond (error (concat ["Equations for ", equationName x, " have different numbers of arguments"])) (Function (equationName x) (patternCount x) (x:xs)) (all (((==) (patternCount x)) . patternCount) xs)
makeFunctions_g (Equation name_a _ _ _) (Equation name_b _ _ _) = listEq (==) name_a name_b
lookupCon name (Environment c _) = lookup name c
lookupVar name (Environment _ v) = lookup name v
walkPatterns f = walkPatterns_items f VArg
walkPatterns_items f base = concat . zipWith (walkPatterns_f2 f) (map base (countFrom 0))
walkPatterns_f2 f v (PCon name ps) = append (f v (PCon name ps)) (walkPatterns_items f (VItem v) ps)
walkPatterns_f2 f v p              = f v p
compile (Program decls funcs) = concat [header, declareConstructors decls, declareFunctions funcs, boxConstructors decls, boxFunctions funcs, compileConstructors decls, compileFunctions (globalEnv decls funcs) funcs]
globalEnv decls funcs = Environment (append (globalEnv_constructorTags decls) (globalEnv_builtinConstructors)) (append (map (globalEnv_f . functionName) funcs) globalEnv_builtinFunctions)
globalEnv_f name = (P name (VBox name))
globalEnv_constructorTags = concatMap (flip zip (countFrom 0) . map constructorName)
globalEnv_builtinConstructors = [P "Nil" 0, P "Cons" 1, P "P" 0]
globalEnv_builtinFunctions = map globalEnv_f ["add", "sub", "_lt", "_le", "_eq", "_ge", "_gt", "_ne", "_and", "_or", "divMod", "negate", "not", "error"]
localEnv ps (Environment t v) = Environment t (append (walkPatterns localEnv_f ps) v)
localEnv_f v (PVar name) = [P name v]
localEnv_f _ (PCon _ _)  = []
declareFunctions_f [] = ""
declareFunctions_f xs = concat ["static Function ", intercalate ", " xs, ";\n"]
declareConstructors = declareFunctions_f . map ((append "f_") . constructorName) . concat
declareFunctions = declareFunctions_f . map ((append "f_") . functionName)
boxConstructors = concatMap boxConstructors_f . concat
boxConstructors_f (Constructor name n) = boxThing name n
boxFunctions = concatMap boxFunctions_f
boxFunctions_f (Function name n _) = boxThing name n
boxThing name n | n == 0 = concat ["static Box b_", name, " = {0, f_", name, ", NULL};\n"]
boxThing name n = concat ["static Partial p_", name, " = {", showInt n, ", 0, f_", name, "};\n", "static Box b_", name, " = {1, NULL, &p_", name, "};\n"]
compileConstructors = concatMap (concat . zipWith compileConstructors_f (countFrom 0))
compileConstructors_f tag (Constructor name n) = concat ["static void *f_", name, "(Box **args)\n", "{\n", allocate n, "\tv->tag = ", showInt tag, ";\n", concatMap initialize (range 0 (sub n 1)), "\treturn v;\n", "}\n"]
allocate n | n == 0 = "\tValue *v = malloc(sizeof(Value));\n\t(void) args;\n"
allocate n = concat ["\tValue *v = malloc(sizeof(Value) + ", showInt n, " * sizeof(Box*));\n"]
initialize i = concat ["\tv->items[", showInt i, "] = args[", showInt i, "];\n"]
compileFunctions env = concatMap (compileFunction env)
compileFunction env (Function name argc equations) =  concat ["static void *f_", name, "(Box **args)\n", "{\n", concatMap (compileEquation env) equations, "\tNO_MATCH(", name, ");\n", "}\n"]
compileEquation genv (Equation _ patterns guard expr) = compileEquation_a (localEnv patterns genv) patterns guard expr
compileEquation_a env patterns guard expr = compileEquation_b (concat ["\treturn ", compileExpressionStrict env expr, ";\n"]) (append (compilePatterns env patterns) (compileGuard env guard))
compileEquation_b returnExpr preds = condList returnExpr (compileEquation_f returnExpr) preds
compileEquation_f returnExpr xs = concat ["\tif (", intercalate " && " xs, ")\n\t", returnExpr]
compilePatterns env = walkPatterns (compilePatterns_f env)
compilePatterns_f _ _ (PVar name) = []
compilePatterns_f env v (PCon name ps) = compilePatterns_h v name (lookupCon name env)
compilePatterns_h v name (Just n) = [concat ["match(", compileVarInfo v, ",", showInt n, ")"]]
compilePatterns_h v name Nothing  = error (append "Not in scope: data constructor " name)
compileGuard env Nothing     = []
compileGuard env (Just expr) = [concat ["isTrue(", compileExpressionStrict env expr, ")"]]
compileExpressionStrict env (Var name) = concat ["force(", compileVar (lookupVar name env) name, ")"]
compileExpressionStrict _   (Con name) = concat ["force(&b_", name, ")"]
compileExpressionStrict _   (Int s)    = concat ["mkInt(", s, ")"]
compileExpressionStrict _   (Char s)   = concat ["mkInt(", s, ")"]
compileExpressionStrict _   (String s) = concat ["mkString(", s, ")"]
compileExpressionStrict env (Ap f x)   = concat ["apply(", compileExpressionStrict env f, ",", compileExpressionLazy env x, ")"]
compileExpressionLazy env (Var name) = compileVar (lookupVar name env) name
compileExpressionLazy _   (Con name) = concat ["&b_", name, ""]
compileExpressionLazy _   (Int s)    = concat ["box(mkInt(", s, "))"]
compileExpressionLazy _   (Char s)   = concat ["box(mkInt(", s, "))"]
compileExpressionLazy _   (String s) = concat ["box(mkString(", s, "))"]
compileExpressionLazy env (Ap f x)   = concat ["deferApply(", compileExpressionLazy env f, ",", compileExpressionLazy env x, ")"]
compileVar (Just v) _    = compileVarInfo v
compileVar Nothing  name = error (append "Not in scope: " name)
compileVarInfo (VBox name) = append "&b_" name
compileVarInfo (VArg n)    = concat ["args[", showInt n, "]"]
compileVarInfo (VItem v n) = concat ["item(", compileVarInfo v, ",", showInt n, ")"]
header="#include <assert.h>\n#include <stdarg.h>\n#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\ntypedef struct Box Box;\ntypedef struct Value Value;\ntypedef struct Partial Partial;\ntypedef void *Function(Box**);\nstruct Box{int state;Function *func;void*vc;Box*fx[];};\nstruct Value{int tag;Box *items[];};\nstruct Partial{int remaining;int applied;Function *func;Box *args[];};\n#define copy(...)memdup(&(__VA_ARGS__), sizeof(__VA_ARGS__))\n#define countof(...)(sizeof(__VA_ARGS__) / sizeof(*(__VA_ARGS__)))\n#define match(box, expectedTag)(((Value*)force(box))->tag == (expectedTag))\n#define item(box, n)(((Value*)(box)->vc)->items[n])\n#define isTrue(value)(!!*(int*)(value))\n#define NO_MATCH(func)fatal(\"Non-exhaustive patterns in function \" #func)\nstatic void fatal(const char *str){fprintf(stderr,\"*** Exception: %s\\n\", str);exit(EXIT_FAILURE);}\nstatic void *memdup(void *ptr, size_t size){void*ret=malloc(size);memcpy(ret,ptr,size);return ret;}\nstatic void *force(Box *box){switch(box->state){\ncase 0:box->state=2;box->vc=box->func(box->vc);box->state=1;\ncase 1:return box->vc;\ndefault:fatal(\"infinite loop\");}}\nstatic void *apply(Partial*f,Box*x){Partial*f2=malloc(sizeof(Partial)+(f->applied+1)*sizeof(Box*));\nmemcpy(f2->args,f->args,f->applied*sizeof(Box*));f2->args[f->applied]=x;\nif(f->remaining>1){f2->remaining=f->remaining-1;f2->applied=f->applied+1;f2->func=f->func;return f2;\n}else return f->func(f2->args);}\nstatic void*deferApply_cb(Box**a){return apply(force(a[0]),a[1]);}\nstatic Box*deferApply(Box*f,Box*x){\nBox*ret=malloc(sizeof(Box)+2*sizeof(Box*));\nret->state=0;\nret->func=deferApply_cb;\nret->vc=ret->fx;\nret->fx[0]=f;\nret->fx[1]=x;\nreturn ret;}\n\nstatic Box*defer(Function*func,void*ctx){\nBox*ret=malloc(sizeof(Box));\nret->state=0;\nret->func=func;\nret->vc=ctx;\nreturn ret;}\n\nstatic Box *box(void *value)\n{\n\tBox *ret = malloc(sizeof(Box));\n\tret->state = 1;\n\tret->func = NULL;\n\tret->vc = value;\n\treturn ret;\n}\n\nstatic int *mkInt(int n)\n{\n\tint *ret = malloc(sizeof(*ret));\n\t*ret = n;\n\treturn ret;\n}\n\nstatic Function f_Nil, f_Cons, f_P;\nstatic Box b_Nil, b_Cons, b_P, b_main;\n\n#define FUNCTION(name, argc) \\\n\tstatic Function f_##name; \\\n\tstatic Partial p_##name = {argc, 0, f_##name}; \\\n\tstatic Box b_##name = {1, NULL, &p_##name}; \\\n\tstatic void *f_##name(Box **args)\n\n#define intop(name, expr) \\\n\tFUNCTION(name, 2) \\\n\t{ \\\n\t\tint a = *(int*)force(args[0]); \\\n\t\tint b = *(int*)force(args[1]); \\\n\t\treturn mkInt(expr); \\\n\t}\n\n#define intop1(name, expr) \\\n\tFUNCTION(name, 1) \\\n\t{ \\\n\t\tint a = *(int*)force(args[0]); \\\n\t\treturn mkInt(expr); \\\n\t}\n\nintop(add,  a + b)\nintop(sub,  a - b)\n\nintop(_lt,  a <  b)\nintop(_le,  a <= b)\nintop(_eq,  a == b)\nintop(_ge,  a >= b)\nintop(_gt,  a >  b)\nintop(_ne,  a != b)\nintop(_and, a && b)\nintop(_or,  a || b)\n\nintop1(negate, -a)\nintop1(not,    !a)\n\nFUNCTION(divMod, 2)\n{\n\tint n = *(int*)force(args[0]);\n\tint d = *(int*)force(args[1]);\n\tint div = n / d;\n\tint mod = n % d;\n\t\n\tif ((mod < 0 && d > 0) || (mod > 0 && d < 0)) {\n\t\tdiv--;\n\t\tmod += d;\n\t}\n\t\n\tBox *pair[2] = {box(mkInt(div)), box(mkInt(mod))};\n\treturn f_P(pair);\n}\n\nstatic void *mkString(const char *str)\n{\n\tif (*str != '\\0') {\n\t\tBox *cons[2] =\n\t\t\t{box(mkInt(*str)), defer((Function*) mkString, (void*)(str + 1))};\n\t\treturn f_Cons(cons);\n\t} else {\n\t\treturn force(&b_Nil);\n\t}\n}\n\nstatic void putStr(Value *v, FILE *f)\n{\n\tif (v->tag == 1) {\n\t\tint c = *(int*)force(v->items[0]);\n\t\tputc(c, f);\n\t\tputStr(force(v->items[1]), f);\n\t}\n}\n\nFUNCTION(error, 1)\n{\n\tfflush(stdout);\n\tfputs(\"*** Exception: \", stderr);\n\tputStr(force(args[0]), stderr);\n\tputc('\\n', stderr);\n\texit(EXIT_FAILURE);\n}\n\nstruct mkStringFromFile\n{\n\tFILE *f;\n\tconst char *name;\n};\n\nstatic void *mkStringFromFile(struct mkStringFromFile *ctx)\n{\n\tint c = fgetc(ctx->f);\n\t\n\tif (c == EOF) {\n\t\tif (ferror(ctx->f))\n\t\t\tperror(ctx->name);\n\t\treturn force(&b_Nil);\n\t}\n\t\n\tBox *cons[2] = {box(mkInt(c)), defer((Function*) mkStringFromFile, ctx)};\n\treturn f_Cons(cons);\n}\n\nint main(void)\n{\n\tstruct mkStringFromFile c_in = {stdin, \"<stdin>\"};\n\tBox *b_in = defer((Function*) mkStringFromFile, copy(c_in));\n\tputStr(apply(force(&b_main), b_in), stdout);\n\treturn 0;\n}\n"
prelude = P 0 "_apply f x=f x;_compose f g x=f(g x);data List a=Nil|Cons a(List a);data P a b=P a b;data B=B;data Maybe a=Nothing|Just a;data Bool=False|True;id x=x;const x _=x;flip f x y=f y x;foldl f z[]=z;foldl f z(x:xs)=foldl f(f z x)xs;foldl1 f(x:xs)=foldl f x xs;foldl1 _[]=error\"foldl1: empty list\";foldr f z[]=z;foldr f z(x:xs)=f x(foldr f z xs);foldr1 f[x]=x;foldr1 f(x:xs)=f x(foldr1 f xs);foldr1 _[]=error\"foldr1: empty list\";map f[]=[];map f(x:xs)=f x:map f xs;filter p[]=[];filter p(x:xs)|p x=x:filter p xs;filter p(x:xs)=filter p xs;zipWith f(x:xs)(y:ys)=f x y:zipWith f xs ys;zipWith f _ _=[];append[]ys=ys;append(x:xs)ys=x:append xs ys;concat=foldr append[];concatMap f=concat.map f;length[]=0;length(_:l)=add 1(length l);take n _|n<=0=[];take _[]=[];take n(x:xs)=x:take(sub n 1)xs;takeWhile p[]=[];takeWhile p(x:xs)|p x=x:takeWhile p xs;takeWhile _ _=[];dropWhile p[]=[];dropWhile p(x:xs)|p x=dropWhile p xs;dropWhile p xs=xs;span p[]=P[][];span p(x:xs)|p x=span_1 x(span p xs);span p xs=P[]xs;span_1 x(P ys zs)=P(x:ys)zs;break p=span(not.p);reverse=foldl(flip(:))[];groupBy _[]=[];groupBy eq(x:xs)=groupBy_1 x eq(span(eq x)xs);groupBy_1 x eq(P ys zs)=(x:ys):groupBy eq zs;maybe n f Nothing=n;maybe n f(Just x)=f x;all p=foldr(&&)True.map p;intersperse _[]=[];intersperse _[x]=[x];intersperse sep(x:xs)=x:sep:intersperse sep xs;intercalate xs xss=concat(intersperse xs xss);isDigit c=c>='0'&&c<='9';isAlphaNum c=c>='0'&&c<='9'||c>='A'&&c<='Z'||c>='a'&&c<='z';isUpper c=c>='A'&&c<='Z';isLower c=c>='a'&&c<='z';showInt n|n<0='-':showInt(negate n);showInt n|n==0=\"0\";showInt n|n>0=reverse(map(add 48)(showInt_1 n));showInt_1 n|n==0=[];showInt_1 n=showInt_2(divMod n 10);showInt_2(P div mod)=mod:showInt_1 div;lines []=[];lines s=lines_1(break((==)'\\n')s);lines_1(P l[])=[l];lines_1(P l(_:s))=l:lines s;interact=id"
