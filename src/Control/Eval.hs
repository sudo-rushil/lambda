{-|
Module      : Control.Eval
Description : State-threaded evaluation of λ stmts

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Control.Eval
    ( Bindings
    , initBindings
    , eval
    ) where


import           Control.Monad.State (execStateT, get, liftIO, modify)
import qualified Data.Map            as M

import           Control.Cmd         (cmd)
import           Control.Module      (addBinding, initBindings, replace, use)
import           Control.State       (Bindings, Lam)
import           Lambda.Reduce       (reduce)
import           Lambda.Syntax


-- Evaluator

eval :: Bindings    -- initial bindings
     -> [Stmt]       -- list of statements
     -> IO Bindings  -- final bindings in IO
eval state = flip execStateT state . mapM_ eval'


eval' :: Stmt -> Lam ()
eval' (Bind name expr) = modify (addBinding name expr)
eval' (Exp expr) = get >>= (liftIO . putStrLn . printExpr . flip evalExpr expr)
eval' (Use file) = use file
eval' (Cmd command expr) = cmd command expr


-- State-aware evaluation of Exprs

evalExpr :: Bindings -> Expr -> Expr
evalExpr bindings = reduce . replace bindings
