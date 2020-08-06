{-|
Module      : Lambda.Syntax
Description : AST definitions for the untyped λ-calculus

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Lambda.Syntax
    ( Expr(..)
    , Stmt(..)
    , Name
    , printExpr
    ) where


-- AST Types

type Name = String


data Expr = Var Name
    | App Expr Expr
    | Abs Name Expr
    deriving (Eq, Ord, Show)


data Stmt = Exp Expr
    | Bind Name Expr
    | Use FilePath
    | Cmd Name Expr
    deriving (Eq, Ord)


-- Instances

instance Show Stmt where
    show (Bind name expr) = "Bind: " ++ name ++ " = " ++ show expr
    show (Exp expr)       = "Expr: " ++ show expr
    show (Use file)       = "Use:  " ++ file
    show (Cmd name expr)  = "Cmd:  " ++ name ++ " " ++ show expr


printExpr :: Expr -> String
printExpr (Var str)        = str
printExpr (Abs str expr)   = "(λ" ++ str ++ "." ++ printExpr expr ++ ")"
printExpr (App expr expr') = "(" ++ printExpr expr ++ " " ++ printExpr expr' ++ ")"
