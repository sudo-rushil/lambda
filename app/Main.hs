module Main where


import           Control.Args
import           Control.Repl (repl)
import           Control.Run  (run)


main :: IO ()
main = do
    opts <- execParser mainParser
    case opts of
        Repl debug -> repl debug
        Run file   -> run file
