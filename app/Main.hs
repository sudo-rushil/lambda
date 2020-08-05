module Main where


import           Control.Monad.Trans        (liftIO)
import qualified Data.ByteString.Lazy.Char8 as B
import           Options.Applicative
import           System.Console.Haskeline

import           Lambda


main :: IO ()
main = do
    file <- B.pack <$> readFile "test/stdlib.lc"
    case parse file of
        Left err -> print err
        Right ex -> do
            print ex
            run initBindings ex
            return ()

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
--                 Just input -> (liftIO $ func env input) >>= loop func


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


-- Command line options

data Options = Options
    { debug :: Bool
    }


mainParser :: ParserInfo Options
mainParser = info (optionsParser <**> helper)
    ( fullDesc
    <> progDesc "Lambda calculus REPL"
    <> header "lambda - untyped lambda calculus interpreter"
    )


optionsParser :: Parser Options
optionsParser = Options
    <$> switch
        ( long "debug"
        <> short 'd'
        <> help "Run interpreter in debug mode"
        )
