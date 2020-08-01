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
    ) where


type Name = String


data Expr = Var Name
    | App Expr Expr
    | Abs Name Expr
    deriving (Eq, Ord)


data Stmt = Exp Expr
    | Bind Name Expr
    deriving (Eq, Ord)
