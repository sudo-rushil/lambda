{-|
Module      : Lambda.Reduce
Description : Reduction operations for λ terms

License     : MIT
Maintainer  : Rushil Mallarapu
-}

module Lambda.Reduce
    ( Bindings
    , initBindings
    , run
    ) where


import           Control.Monad.State (StateT, execStateT, get, liftIO, modify)
import qualified Data.Map            as M
import qualified Data.Set            as S


import           Lambda.Syntax


-- Evaluation types

type Bindings = M.Map Name Expr


type Lam a = StateT Bindings IO a


-- Evaluator

eval :: Stmt -> Lam ()
eval (Bind nme expr) = modify addBinding
    where
        addBinding bindings = M.insert nme (replace bindings expr) bindings
eval (Exp expr) = get >>= (liftIO . (\s -> putStrLn $ " " ++ printExpr s) . flip eval' expr)
eval _ = return ()


run :: Bindings -> [Stmt] -> IO Bindings
run state = flip execStateT state . mapM_ eval


initBindings :: Bindings
initBindings = M.fromList primitiveBindings


primitiveBindings :: [(Name, Expr)]
primitiveBindings =
    [ ("0", Abs "f" (Abs "x" (Var "x")))
    , ("1", Abs "f" (Abs "x" (App (Var "f") (Var "x"))))
    , ("succ",  Abs "n" (Abs "f" (Abs "x" (App (Var "f") (App (App (Var "n") (Var "f")) (Var "x"))))))
    , ("#t", Abs "t" (Abs "f" (Var "t")))
    , ("#f", Abs "t" (Abs "f" (Var "f")))
    , ("and", Abs "p" (Abs "q" (App (App (Var "p") (Var "q"))  (Var "p"))))
    , ("or", Abs "p" (Abs "q" (App (App (Var "p") (Var "p"))  (Var "q"))))
    ]


-- Evaluation of lambda expressions

eval' :: Bindings -> Expr -> Expr
eval' bindings = reduce . replace bindings


replace :: Bindings -> Expr -> Expr
replace bindings (Var nme) =
    case bindings M.!? nme of
        Nothing   -> Var nme
        Just expr -> expr
replace bindings (App expr expr') =
    App (replace bindings expr) (replace bindings expr')
replace bindings (Abs nme expr) =
    Abs nme (replace bindings expr)


reduce :: Expr -> Expr
reduce expr
    | expr == expr' = expr
    | otherwise = reduce expr'
    where
        expr' = betaReduce expr


-- Helper functions necessary for operations

betaReduce :: Expr -> Expr
betaReduce (App (Abs name expr) expr') = substitute name expr' expr
betaReduce (App expr expr') = App (betaReduce expr) (betaReduce expr')
betaReduce (Abs name expr) = Abs name $ betaReduce expr
betaReduce expr = expr


substitute :: Name -> Expr -> Expr -> Expr
substitute var expr (Var name)
    | name == var   = expr
    | otherwise     = Var name
substitute var expr (App expr' expr'') = App nexpr nexpr'
    where
        nexpr = substitute var expr expr'
        nexpr' = substitute var expr expr''
substitute var expr (Abs name expr')
    | name == var           = Abs name expr'
    | name `S.notMember` fv = Abs name $ substitute var expr expr'
    | otherwise =
        let var' = varsupply (S.insert var $ freevars (App expr expr'))
        in Abs var' $ (substitute var expr . substitute name (Var var')) expr'
    where
        fv = freevars expr


freevars :: Expr -> S.Set Name
freevars (Var str)        = S.singleton str
freevars (Abs str expr)   = freevars expr `S.difference` S.singleton str
freevars (App expr expr') = freevars expr `S.union` freevars expr'


varsupply :: S.Set Name -> Name
varsupply freevars = head [ "x'" ++ show i | i <- [1..], ("x" ++ show i) `S.notMember` freevars]


-- Instances

instance Show Stmt where
    show (Bind name expr) = "Bind: " ++ name ++ " = " ++ show expr
    show (Exp expr)       = "Expr: " ++ show expr
    show (Use file)       = "Use:  " ++ file
    show (Cmd name expr)  = "Cmd:  " ++ name ++ " " ++ show expr


printExpr :: Expr -> String
printExpr (Var str)        = str
printExpr (Abs str expr)   = "(λ" ++ str ++ "." ++ printExpr expr ++ ")"
printExpr (App expr expr') = "(" ++ printExpr expr ++ " " ++ printExpr expr' ++ ")"
