module Main where


import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           System.Console.Haskeline

-- import           Lambda                     (parse, pprint')
import           Lambda


process :: String -> IO ()
process line = do
        let res = parse $ B.pack line
        case res of
            Left err -> print err
            Right ex -> do
                print ex
                case ex of
                    (Bind _ _) -> return ()
                    (Exp expr) -> do
                        putStrLn $ pprint expr
                        putStrLn $ pprint $ reduce expr


main :: IO ()
main = runInputT defaultSettings loop
    where
        loop = do
            minput <- getInputLine " λ> "
            case minput of
                Nothing    -> outputStrLn "Exiting..."
                Just input -> (liftIO $ process input) >> loop
