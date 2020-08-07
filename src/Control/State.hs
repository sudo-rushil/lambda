{-|
Module      : Control.State
Description : State types for bindings and modules in lambda

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Control.State
    ( Lam
    , Bindings
    , evalExpr
    , replace
    ) where


import           Control.Monad.State (StateT)
import           Data.Map.Strict     (Map, (!?))

import           Lambda.Reduce       (reduce)
import           Lambda.Syntax       (Expr (..), Name)


-- State threading types

type Bindings = Map Name Expr


type Lam a = StateT Bindings IO a


-- State-aware evaluation of Exprs

evalExpr :: Bindings -> Expr -> Expr
evalExpr bindings = reduce . replace bindings


replace :: Bindings -> Expr -> Expr
replace bindings (Var name) =
    case bindings !? name of
        Nothing   -> Var name
        Just expr -> expr
replace bindings (App expr expr') =
    App (replace bindings expr) (replace bindings expr')
replace bindings (Abs name expr) =
    Abs name (replace bindings expr)
