{-|
Module      : Lambda.Reduce
Description : Reduction operations for λ terms

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Lambda.Reduce where
    -- ( pprint'
    -- , freevars'
    -- ) where


import qualified Data.Set      as S


import           Lambda.Syntax


freevars :: Expr -> S.Set Name
freevars (Var str)        = S.singleton str
freevars (Abs str expr)   = freevars expr `S.difference` S.singleton str
freevars (App expr expr') = freevars expr `S.union` freevars expr'


-- Reduce to irreducible form
reduce :: Expr -> Expr
reduce expr
    | expr == expr' = expr
    | otherwise = reduce expr'
    where
        expr' = betaReduce expr


-- Capture avoiding substitution
substitute :: Name -> Expr -> Expr -> Expr
substitute var expr (Var name)
    | name == var   = expr
    | otherwise     = Var name
substitute var expr (App expr' expr'') = App nexpr nexpr'
    where
        nexpr = substitute var expr expr'
        nexpr' = substitute var expr expr''
substitute var expr (Abs name expr')
    | name == var           = Abs name expr'
    | name `S.notMember` fv = Abs name $ substitute var expr expr'
    | otherwise =
        let var' = varsupply (S.insert var $ freevars (App expr expr'))
        in Abs var' $ (substitute var expr . substitute name (Var var')) expr'
    where
        fv = freevars expr


varsupply :: S.Set Name -> Name
varsupply freevars = head [ "x'" ++ show i | i <- [1..], ("x" ++ show i) `S.notMember` freevars]


betaReduce :: Expr -> Expr
betaReduce (App (Abs name expr) expr') = substitute name expr' expr
betaReduce (App expr expr') = App (betaReduce expr) (betaReduce expr')
betaReduce (Abs name expr) = Abs name $ betaReduce expr
betaReduce expr = expr


-- Pretty printing

instance Show Stmt where
    show (Bind (Var nme) expr) = "Bind: " ++ nme ++ " = " ++ show expr
    show (Exp expr)            = "Expr: " ++ show expr


instance Show Expr where
    show (Var str)        = str
    show (Abs str expr)   = "(λ" ++ str ++ "." ++ show expr ++ ")"
    show (App expr expr') = "(" ++ show expr ++ " " ++ show expr' ++ ")"
