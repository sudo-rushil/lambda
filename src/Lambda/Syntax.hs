module Lambda.Syntax
    ( Expr(..)
    , Stmt(..)
    ) where


data Expr = Var String
    | App Expr Expr
    | Abs String Expr
    | Brack Expr
    deriving (Eq, Ord, Show)

data Stmt = Exp Expr
    | Bind String Expr
    deriving (Eq, Ord, Show)
