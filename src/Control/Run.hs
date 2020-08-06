{-|
Module      : Control.Run
Description : File interpreter for lambda

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Control.Run where


import qualified Data.ByteString.Lazy.Char8 as B

import           Control.Eval               (eval, initBindings)
import           Lambda.Parse               (parse)
import           Lambda.Syntax              (Stmt)


-- Run file starting from init bindings

run :: FilePath -> IO ()
run file = readLC file >>= eval initBindings >> return ()


readLC :: FilePath -> IO [Stmt]
readLC file = do
    content <- B.readFile file
    case parse content of
        Left err    -> print err >> return []
        Right stmts -> mapM_ print stmts >> return stmts
