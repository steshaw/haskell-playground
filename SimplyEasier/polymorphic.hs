
type Sym = String

data Expr
  = Var Sym
  | App Expr Expr
  | Lam Sym Type Expr
  | TLam Sym Kind Expr
  | TApp Expr Type
  deriving (Eq, Read, Show)

data Type
  = Arrow Type Type
  | TVar Sym
  deriving (Eq, Read, Show)

data Kind
  = KArrow Kind Kind
  | Star
  deriving (Eq, Read, Show)
