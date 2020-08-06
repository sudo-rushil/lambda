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


import           System.Directory (makeAbsolute, withCurrentDirectory)
import           System.FilePath  (takeDirectory)

import           Control.Eval     (eval, initBindings)
import           Control.Module   (readLC)


-- Run file starting from init bindings

run :: FilePath -> IO ()
run file = do
    runDir <- takeDirectory <$> makeAbsolute file
    contents <- readLC file
    withCurrentDirectory runDir $ eval initBindings contents
    return ()
