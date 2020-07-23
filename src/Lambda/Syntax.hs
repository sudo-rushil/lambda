module Lambda.Syntax
    ( Expr(..)
    ) where


data Expr = Float Double
    | Var String
    | Call Name [Expr]
    | Function Name [Expr] Expr
    | Extern Name [Expr]
    deriving (Eq, Ord, Show)

type Name = String
