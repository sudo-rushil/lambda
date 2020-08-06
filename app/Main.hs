module Main where


import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           System.Console.Haskeline

import           Lambda

import           Control.Args
import           Control.Repl               (repl)
import           Control.Run                (run)


main :: IO ()
main = do
    opts <- execParser mainParser
    case opts of
        Repl debug -> repl debug
        Run file   -> run file
