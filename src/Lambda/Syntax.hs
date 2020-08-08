{-|
Module      : Lambda.Syntax
Description : AST definitions for the untyped λ-calculus

License     : MIT
Maintainer  : Rushil Mallarapu
-}

{-# LANGUAGE GADTs #-}


module Lambda.Syntax
    ( Expr(..)
    , Stmt(..)
    , Name
    , printExpr
    ) where


-- AST Types

type Name = String


data Expr where
    Var :: Name -> Expr
    App :: Expr -> Expr -> Expr
    Abs :: Name -> Expr -> Expr
    deriving (Eq, Ord, Show)


data Stmt where
    Exp :: Expr -> Stmt
    Bind :: Name -> Expr -> Stmt
    Use :: FilePath -> Stmt
    Cmd :: Name -> Expr -> Stmt
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
