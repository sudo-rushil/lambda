module Main where


import           Control.Monad.Trans      (liftIO)
import           System.Console.Haskeline

import           Lambda


process :: String -> IO ()
process line = do
        let res = Right line
        case res of
            Left _   -> print ""
            Right ex -> mapM_ print ex


main :: IO ()
main = runInputT defaultSettings loop
    where
        loop = do
            minput <- getInputLine " λ> "
            case minput of
                Nothing    -> outputStrLn "Exiting..."
                Just input -> (liftIO $ process input) >> loop
