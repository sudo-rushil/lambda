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
import           Data.List                  (foldl')
import qualified Data.Map.Strict            as M

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
        newBindings = foldl' (\acc (Bind name expr) -> addBinding name expr acc)


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


-- Primitive bindings

initBindings :: IO Bindings
initBindings = do
    stdlib <- getDataFileName "stdlib.lc"
    bindings <- execStateT (use stdlib) M.empty
    return bindings
