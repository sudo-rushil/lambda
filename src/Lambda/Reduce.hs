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


freevars :: Expr -> S.Set String
freevars (Var str)        = S.singleton str
freevars (Abs str expr)   = freevars expr `S.difference` S.singleton str
freevars (App expr expr') = freevars expr `S.union` freevars expr'


-- rough alpha conversion
rename :: String -> String -> Expr -> Expr
rename var nvar expr@(Var str)
    | str == var = Var nvar
    | otherwise = expr
rename var nvar (Abs str expr)
    | str == var = Abs nvar nexpr
    | otherwise = (Abs str nexpr)
    where
        nexpr = rename var nvar expr
rename var nvar (App expr expr') = App nexpr nexpr'
    where
        nexpr = rename var nvar expr
        nexpr' = rename var nvar expr'
