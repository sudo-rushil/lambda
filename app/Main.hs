module Main where


import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           System.Console.Haskeline

import           Lambda


process :: Bindings -> String -> IO Bindings
process bindings line = do
        let res = parse $ B.pack line
        case res of
            Left err -> print err >> putStrLn "" >> return bindings
            Right ex -> do
                bindings' <- run bindings ex
                putStrLn ""
                return bindings'



main :: IO ()
main = runInputT defaultSettings (loop initBindings)
    where
        loop env = do
            minput <- getInputLine " λ> "
            case minput of
                Nothing    -> outputStrLn "Exiting..."
                Just input -> (liftIO $ process env input) >>= loop
