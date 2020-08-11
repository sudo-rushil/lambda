{-|
Module      : Combinator.Transform
Description : Transform between λ-stmts and combinator calculus

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Combinator.Transform
    ( transform
    ) where


import           Control.Monad       ((<=<))
import           Control.Monad.State (StateT, execStateT, get, lift, modify,
                                      put)
import           Data.List           (foldl')

import           Combinator.Convert  (convert)
import           Combinator.Syntax
import           Control.Module      (addBinding, filterBinds, initBindings,
                                      readLC, replace)
import           Control.State       (Bindings, evalExpr)
import           Lambda.Syntax


-- Program transformation binding monad

type Transform a = StateT TransformState IO a


data TransformState = TS
    { binds :: Bindings
    , terms :: [Term]
    }
    deriving (Eq, Show)


-- Transform Stmts to expanded list of Terms

transform :: [Stmt] -> IO [Term]
transform stmts = do
    inits <- initBindings
    expand (TS inits []) stmts


expand :: TransformState -> [Stmt] -> IO [Term] -- ignore cmds just for now
expand state = return . terms <=< flip execStateT state . mapM_ expand'


expand' :: Stmt -> Transform ()
expand' (Bind name expr) = modify $ transformBind name expr
expand' (Exp expr)       = modify $ transformExp expr
expand' (Use file)       = transformUse file
expand' _                = return ()


-- Functions for modifying transformation state

transformBind :: Name -> Expr -> TransformState -> TransformState
transformBind name expr (TS bindings terms) = TS (addBinding name expr bindings) terms


transformExp :: Expr -> TransformState -> TransformState
transformExp expr (TS bindings terms) = TS bindings terms'
    where
        expr' = replace bindings expr
        terms' = (convert expr') : terms


transformUse :: FilePath -> Transform ()
transformUse file = do
    imports <- lift $ filterBinds <$> readLC file
    (TS bindings terms) <- get
    let bindings' = newBindings bindings imports
    put $ TS bindings' terms
    return ()
    where
        newBindings = foldl' (\acc (Bind name expr) -> addBinding name expr acc)
