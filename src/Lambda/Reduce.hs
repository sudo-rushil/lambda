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


pprint' :: Stmt -> String
pprint' (Bind _ _) = error "not implemented"
pprint' (Exp expr) = (init.tail.pprint) expr


pprint :: Expr -> String
pprint (Var str)        = str
pprint (Abs str expr)   = "(λ" ++ str ++ "." ++ pprint expr ++ ")"
pprint (App expr expr') = "(" ++ pprint expr ++ " " ++ pprint expr' ++ ")"


freevars :: Expr -> S.Set Name
freevars (Var str)        = S.singleton str
freevars (Abs str expr)   = freevars expr `S.difference` S.singleton str
freevars (App expr expr') = freevars expr `S.union` freevars expr'


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
varsupply freevars = head [ "x'" ++ show i | i <- [1..], ("x'" ++ show i) `S.notMember` freevars]
