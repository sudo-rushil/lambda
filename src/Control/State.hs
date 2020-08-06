{-|
Module      : Control.State
Description : State types for bindings and modules in lambda

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Control.State
    ( Lam
    , Bindings
    ) where


import           Control.Monad.State (StateT)
import           Data.Map.Strict     (Map)

import           Lambda.Syntax       (Expr, Name)


-- State threading types

type Bindings = Map Name Expr


type Lam a = StateT Bindings IO a
