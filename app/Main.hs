module Main where


import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           System.Console.Haskeline

import           Lambda

import           Control.Args
import           Control.Eval
import           Control.Repl               (repl)


main :: IO ()
main = do
    opts <- execParser mainParser
    case opts of
        Repl debug -> repl debug
        Run _   -> do
            file <- B.readFile "test/stdlib.lc"
            case parse file of
                Left err -> print err
                Right ex -> do
                    mapM_ print ex
                    eval initBindings ex
                    return ()
             -- run file
-- main :: IO ()
-- main = do
--
