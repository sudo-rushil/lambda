{-|
Module      : Control.Eval
Description : State-threaded evaluation of λ stmts

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Control.Eval
    ( Bindings
    , initBindings
    , run
    ) where


import           Control.Monad.State (StateT, execStateT, get, liftIO, modify)
import qualified Data.Map            as M

import           Lambda.Reduce       (reduce)
import           Lambda.Syntax


-- State threading types

type Bindings = M.Map Name Expr


type Lam a = StateT Bindings IO a


-- Evaluator

run :: Bindings     -- initial bindings
    -> [Stmt]       -- list of statements
    -> IO Bindings  -- final bindings in IO
run state = flip execStateT state . mapM_ eval


eval :: Stmt -> Lam ()
eval (Bind name expr) = modify addBinding
    where
        addBinding bindings = M.insert name (replace bindings expr) bindings
eval (Exp expr) = get >>= (liftIO . putStrLn . printExpr . flip eval' expr)
eval _ = return () -- implement use and cmd


-- State-aware evaluation of Exprs

eval' :: Bindings -> Expr -> Expr
eval' bindings = reduce . replace bindings


replace :: Bindings -> Expr -> Expr
replace bindings (Var nme) =
    case bindings M.!? nme of
        Nothing   -> Var nme
        Just expr -> expr
replace bindings (App expr expr') =
    App (replace bindings expr) (replace bindings expr')
replace bindings (Abs nme expr) =
    Abs nme (replace bindings expr)


-- Primitive bindings -- will put in a stdlib.lc

initBindings :: Bindings
initBindings = M.fromList primitiveBindings


primitiveBindings :: [(Name, Expr)]
primitiveBindings =
    [ ("0", Abs "f" (Abs "x" (Var "x")))
    , ("1", Abs "f" (Abs "x" (App (Var "f") (Var "x"))))
    , ("succ",  Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))))))
    , ("#t", Abs "t" (Abs "f" (Var "t")))
    , ("#f", Abs "t" (Abs "f" (Var "f")))
    , ("and", Abs "p" (Abs "q" (App (App (Var "p") (Var "q"))  (Var "p"))))
    , ("or", Abs "p" (Abs "q" (App (App (Var "p") (Var "p"))  (Var "q"))))
    ]
