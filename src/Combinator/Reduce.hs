{-|
Module      : Combinator.Reduce
Description : Reductions of combinator terms

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Combinator.Reduce
    ( reduce
    ) where


import           Combinator.Syntax


-- Reduction of combinator terms

reduce :: Term -> Term
reduce term
    | term == term' = term
    | otherwise = reduce term'
    where
        term' = reduce' term


-- Single reduction of combinator terms

reduce' :: Term -> Term
reduce' (A (A (A (C S) x) y) z) = A (A x z) (A y z)
reduce' (A (A (C K) x) y)       = x
reduce' (A (C I) x)             = x
reduce' (A term term')
    | reducible term = A (reduce' term) term'
    | otherwise = A term (reduce' term')
    where
reduce' term                      = term


reducible :: Term -> Bool
reducible (A _ _) = True
reducible _       = False
