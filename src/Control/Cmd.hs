{-|
Module      : Control.Cmd
Description : Special commands for λ stmts

License     : MIT
Maintainer  : Rushil Mallarapu
-}

{-# LANGUAGE OverloadedStrings #-}


module Control.Cmd
    ( cmd
    , findExpr
    ) where


import           Control.Monad.State        (get, lift, liftIO, modify)
import           Data.ByteString.Lazy.Char8 (pack)
import qualified Data.Map                   as M

import           Control.Module             (addBinding)
import           Control.State              (Bindings, Lam, evalExpr)
import           Lambda.Parse               (parse)
import           Lambda.Syntax


-- Commands for defining useful extra behavior

type Cmd = Expr -> Lam ()


cmd :: Name -> Expr -> Lam ()
cmd name expr =
    case lookup name cmdlist of
        Nothing       -> liftIO $ putStrLn ("Error: command \"" ++ name ++ "\" not found")
        Just cmdToRun -> cmdToRun expr


cmdlist :: [(Name, Cmd)]
cmdlist =
    [ ( ":ast", astCmd )
    , ( ":get", getCmd )
    , ( ":put", putCmd )
    ]


-- Actual command definitions

-- Print out AST of expr
astCmd :: Cmd
astCmd expr = liftIO (print expr)


-- Read input value and bind it to a variable
getCmd :: Cmd
getCmd (Var name) = do
    (inp:_) <- lift $ words <$> getLine
    expr <- lift $ parseExpr inp
    modify (addBinding name expr)
getCmd _ = liftIO (putStrLn "Error: not a variable")


parseExpr :: String -> IO Expr
parseExpr str =
    case parse $ pack str <> "\n" of
        Left err    -> print err >> return (Var "_")
        Right (Exp expr : _) -> return expr
        Right _ -> putStrLn "Error: not an expression" >> return (Var "_")


-- Lookup expr in bindings and print its binding key
putCmd :: Cmd
putCmd expr = do
    bindings <- get
    let expr' = evalExpr bindings expr
    case findExpr bindings expr' of
        Nothing   -> liftIO (putStrLn $ printExpr expr)
        Just name -> liftIO (putStrLn name)



findExpr :: Bindings -> Expr -> Maybe Name
findExpr bindings expr = lookup expr reversed
    where
        reversed = map swap (M.toList bindings)
        swap (a, b) = (b, a)
