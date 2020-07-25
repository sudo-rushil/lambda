{-|
Module      : Lambda.Reduce
Description : Reduction operations for λ terms

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Lambda.Reduce
    ( eval'
    ) where


import           Lambda.Syntax


eval' :: Stmt -> String
eval' (Bind _ _) = error "not implemented"
eval' (Exp expr) = eval expr


eval :: Expr -> String
eval (Var str)        = str
eval (Abs str expr)   = "(λ" ++ str ++ "." ++ eval expr ++ ")"
eval (App expr expr') = "(" ++ eval expr ++ " " ++ eval expr' ++ ")"
