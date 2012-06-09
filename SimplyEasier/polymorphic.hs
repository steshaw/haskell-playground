
type Sym = String

data Expr
  = Var Sym
  | App Expr Expr
  | Lam Sym Type Expr
  | TLam Sym Kind Expr
  | TApp Expr Type
  deriving (Eq, Read, Show)

data Type
  = TVar Sym
  | Arrow Type Type
  | TLam Sym Kind Type
  | TApp Type Type
  deriving (Eq, Read, Show)

data Kind
  = Star
  | KArrow Kind Kind
  deriving (Eq, Read, Show)

