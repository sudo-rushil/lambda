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
    , File
    ) where


type Name = String
type File = String


data Expr = Var Name
    | App Expr Expr
    | Abs Name Expr
    deriving (Eq, Ord, Show)


data Stmt = Exp Expr
    | Bind Name Expr
    | Use File
    | Cmd Name Expr
    deriving (Eq, Ord)
