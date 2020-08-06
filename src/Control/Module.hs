{-|
Module      : Control.Module
Description : Use statement and bind semantics for lambda

License     : MIT
Maintainer  : Rushil Mallarapu
-}

{-# LANGUAGE LambdaCase #-}


module Control.Module
    ( use
    , replace
    , readLC
    , addBinding
    , initBindings
    ) where

import           Control.Monad.State        (execStateT, get, lift, put)
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map                   as M

import           Control.State              (Bindings, Lam)
import           Lambda.Parse               (parse)
import           Lambda.Syntax

import           Paths_lambda               (getDataFileName)


-- Use operation to pull in bindings from external files

use :: FilePath -> Lam ()
use file = do
    imports <- lift $ filterBinds <$> readLC file
    bindings <- get
    put $ newBindings bindings imports
    return ()
    where
        newBindings = foldr (\(Bind name expr) acc -> addBinding name expr acc)


filterBinds :: [Stmt] -> [Stmt]
filterBinds = filter (\case {Bind _ _ -> True; _ -> False})


-- File operations

readLC :: FilePath -> IO [Stmt]
readLC file = do
    content <- B.readFile file
    case parse content of
        Left err    -> print err >> return []
        Right stmts -> return stmts


-- Bind operations

addBinding :: Name -> Expr -> Bindings -> Bindings
addBinding name expr bindings = M.insert name (replace bindings expr) bindings


replace :: Bindings -> Expr -> Expr
replace bindings (Var name) =
    case bindings M.!? name of
        Nothing   -> Var name
        Just expr -> expr
replace bindings (App expr expr') =
    App (replace bindings expr) (replace bindings expr')
replace bindings (Abs name expr) =
    Abs name (replace bindings expr)


-- Primitive bindings -- will put in a stdlib.lc

initBindings :: IO Bindings
initBindings = do
    stdlib <- getDataFileName "stdlib.lc"
    bindings <- execStateT (use stdlib) M.empty
    print bindings
    return bindings



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
