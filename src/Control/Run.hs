{-|
Module      : Control.Run
Description : File interpreter for lambda

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Control.Run
    ( run
    , readLC
    ) where


import           Control.Eval   (eval, initBindings)
import           Control.Module (readLC)


-- Run file starting from init bindings

run :: FilePath -> IO ()
run file = readLC file >>= eval initBindings >> return ()
