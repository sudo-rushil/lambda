{-|
Module      : Control.Args
Description : Argument parsing for lambda interpreter

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Control.Args
    ( execParser
    , mainParser
    , Options(..)
    ) where


import           Options.Applicative


-- Command line option types

data Options = Run
    { fileName :: FilePath
    }
    | Repl
    { debug :: Bool
    }
    deriving (Show)


-- Parsers

mainParser :: ParserInfo Options
mainParser = info (optionsParser <**> helper)
    ( fullDesc
    <> progDesc "Run single file or enter REPL"
    <> header "lambda - untyped lambda calculus interpreter"
    )


optionsParser :: Parser Options
optionsParser = runParser <|> replParser

runParser :: Parser Options
runParser = Run
    <$> argument str (metavar "FILE")


replParser :: Parser Options
replParser = Repl
    <$> switch
        ( long "debug"
        <> short 'd'
        <> help "Run REPL in debug mode"
        )
