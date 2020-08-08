{-|
Module      : Combinator.Syntax
Description : AST definitions for the SKI combinator calculus

License     : MIT
Maintainer  : Rushil Mallarapu
-}

{-# LANGUAGE GADTs #-}


module Combinator.Syntax
    ( Term(..)
    , Comb(..)
    , printTerm
    ) where


import           Lambda.Syntax (Name)


-- Combinator Term Types

data Term where
    V :: Name -> Term
    C :: Comb -> Term
    A :: Term -> Term -> Term
    deriving (Eq, Show)


data Comb where
    S :: Comb
    K :: Comb
    I :: Comb
    deriving (Eq, Show)


-- Instances

printTerm :: Term -> String
printTerm (V name) = name
printTerm (C comb) = show comb
printTerm (A term term') = "(" ++ str ++ " " ++ str' ++ ")"
    where
        str = printTerm term
        str' = printTerm term'
