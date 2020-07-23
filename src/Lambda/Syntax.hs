{-|
Module      : Lambda.Syntax
Description : AST definitions for the untyped λ-calculus

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Lambda.Syntax
    ( Expr(..)
    , Stmt(..)
    ) where


data Expr = Var String
    | App Expr Expr
    | Abs String Expr
    deriving (Eq, Ord, Show)


data Stmt = Exp Expr
    | Bind Expr Expr
    deriving (Eq, Ord, Show)
