module Main where


import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
-- import           Options.Applicative
import           System.Console.Haskeline

import           Lambda

import           Control.Args


main = do
    opts <- execParser mainParser
    print opts
-- main :: IO ()
-- main = do
--     file <- B.readFile "test/stdlib.lc"
--     case parse file of
--         Left err -> print err
--         Right ex -> do
--             mapM_ print ex
--             run initBindings ex
--             return ()

-- main :: IO ()
-- main = do
--         Options d <- execParser mainParser
--         if (not d)
--             then runInputT defaultSettings (loop process initBindings)
--             else putStrLn "Running in debug mode" >> runInputT defaultSettings (loop debugProcess initBindings)
--     where
--         loop func env = do
--             minput <- getInputLine " λ> "
--             case minput of
--                 Nothing    -> outputStrLn "Exiting..."
--                 Just input -> (liftIO $ putStr " " >> func env input) >>= loop func


-- process :: Bindings -> String -> IO Bindings
-- process bindings line = do
--         let res = parse $ B.pack line
--         case res of
--             Left err -> print err >> putStrLn "" >> return bindings
--             Right ex -> do
--                 bindings' <- run bindings ex
--                 putStrLn ""
--                 return bindings'
--
--
-- debugProcess :: Bindings -> String -> IO Bindings
-- debugProcess bindings line = do
--         let res = parse $ B.pack line
--         case res of
--             Left err -> print err >> putStrLn "" >> return bindings
--             Right ex -> do
--                 putStr "" >> print ex
--                 bindings' <- run bindings ex
--                 putStrLn ""
--                 return bindings'
