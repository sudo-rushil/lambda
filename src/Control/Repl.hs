{-|
Module      : Control.Repl
Description : Repl functionality for lambda

License     : MIT
Maintainer  : Rushil Mallarapu
-}

{-# LANGUAGE OverloadedStrings #-}


module Control.Repl
    ( repl
    ) where


import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           System.Console.Haskeline

import           Control.Eval               (Bindings, eval, initBindings)
import           Lambda.Parse               (parse)


-- Run REPL starting from init bindings

repl :: Bool -> IO ()
repl debug = runInputT defaultSettings (loop debug process initBindings)


loop :: Bool -- debug flag
    -> (Bool -> Bindings -> String -> IO Bindings) -- process
    -> Bindings -- threaded bindings
    -> InputT IO ()
loop debug func env = do
    minput <- getInputLine " λ> "
    case minput of
        Nothing    -> outputStrLn "Exiting..."
        Just input -> (liftIO $ output input) >>= loop debug func
    where
        output inp = putStr " " >> func debug env inp <* putStrLn ""


process :: (Bool -> Bindings -> String -> IO Bindings)
process debug bindings line = do
    case parse $ B.pack line <> "\n" of
        Left err   -> print err >> return bindings
        Right stmt ->
            if debug
                then print (head stmt) >> putStr " " >> eval bindings stmt
                else eval bindings stmt
