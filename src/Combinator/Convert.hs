{-|
Module      : Combinator.Convert
Description : Conversions between λ-calculus and combinator calculus

License     : MIT
Maintainer  : Rushil Mallarapu
-}

{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}


module Combinator.Convert
    ( convert
    ) where


import qualified Data.Set          as S

import           Combinator.Syntax
import           Lambda.Reduce     (reduce)
import           Lambda.Syntax


-- Intermediate type of Exprs and Terms

data IExpr where
    IAbs :: Name -> IExpr -> IExpr
    IVar :: Name -> IExpr
    IApp :: IExpr -> IExpr -> IExpr
    IV   :: Name -> IExpr
    IC   :: Comb -> IExpr
    IA   :: IExpr -> IExpr -> IExpr
    deriving (Eq, Show)


-- Conversion of Exprs to Terms
convert :: Expr -> Term
convert = iexprToTerm . transform' . exprToIExpr


transform' :: IExpr -> IExpr
transform' (IAbs name (IApp expr (IVar name')))
    | not (free name expr) = transform' expr
transform' (IAbs name (IA expr (IVar name')))
    | not (free name expr) = transform' expr
transform' (IAbs name (IApp expr (IV name')))
    | not (free name expr) = transform' expr
transform' (IAbs name (IA expr (IV name')))
    | not (free name expr) = transform' expr
transform' (IAbs name (IApp expr expr'))
    | free name expr || free name expr'
        = IA (IA (IC S) (transform' (IAbs name expr))) (transform' (IAbs name expr'))
transform' (IAbs name (IA expr expr'))
    | free name expr || free name expr'
        = IA (IA (IC S) (transform' (IAbs name expr))) (transform' (IAbs name expr'))
transform' (IAbs name abst@(IAbs _ expr))
    | free name expr = transform' (IAbs name (transform' abst))
transform' (IAbs name expr)
    | not (free name expr) = IA (IC K) (transform' expr)
transform' (IApp expr expr')      = IA (transform' expr) (transform' expr')
transform' (IAbs name (IVar name'))
    | name == name' = IC I
transform' (IAbs name (IV name'))
    | name == name' = IC I
transform' (IVar name)            = IV name
transform' expr = expr


-- Switch between intermediate type and proper types

exprToIExpr :: Expr -> IExpr
exprToIExpr (Abs name expr)  = IAbs name (exprToIExpr expr)
exprToIExpr (App expr expr') = IApp (exprToIExpr expr) (exprToIExpr expr')
exprToIExpr (Var name)       = IVar name


iexprToExpr :: IExpr -> Expr
iexprToExpr (IAbs name expr)  = Abs name (iexprToExpr expr)
iexprToExpr (IApp expr expr') = App (iexprToExpr expr) (iexprToExpr expr')
iexprToExpr (IVar name)       = Var name


termToIExpr :: Term -> IExpr
termToIExpr (V name)       = IV name
termToIExpr (C comb)       = IC comb
termToIExpr (A term term') = IA (termToIExpr term) (termToIExpr term')


iexprToTerm :: IExpr -> Term
iexprToTerm (IV name)       = V name
iexprToTerm (IC comb)       = C comb
iexprToTerm (IA expr expr') = A (iexprToTerm expr) (iexprToTerm expr')


-- True if name is free in expr
free :: Name -> IExpr -> Bool
free name expr = name `S.member` freevars expr


freevars :: IExpr -> S.Set Name
freevars (IApp expr expr') = freevars expr `S.union` freevars expr'
freevars (IA expr expr')   = freevars expr `S.union` freevars expr'
freevars (IAbs name expr)  = freevars expr `S.difference` S.singleton name
freevars (IVar name)       = S.singleton name
freevars (IV name)         = S.singleton name
freevars (IC _)            = S.empty
