module Untyped.Utils where

import Untyped.Expr

idExpr :: Expr
idExpr = Lam "x" (Var "x")

id_ :: Expr -> Expr
id_ = Appl idExpr

-- short hands for calling the full evaluations
cbvEval, cbnEval, noEval :: Expr -> Expr
cbvEval = eval cbvStep
cbnEval = eval cbnStep
noEval = eval noStep

-- display the full evaluation steps.
displayEval :: (Expr -> String) -> (Expr -> Maybe Expr) -> Expr -> IO ()
displayEval displayFunc step e = mapM_ (\ex -> putStrLn (">> " ++ displayFunc ex)) (evalSteps step e)

pretty :: Expr -> String
pretty e = case e of
    Var x -> x
    Lam x body -> "\\" ++ x ++ ". " ++ pretty body
    Appl t1 t2 ->
        case (t1, t2) of
        (Var _, Var _) -> pretty t1 ++ " " ++ pretty t2
        (Var _, _) -> pretty t1 ++ " (" ++ pretty t2 ++ ")"
        (_, Var _) -> "(" ++pretty t1 ++ ") " ++ pretty t2
        (_, _) -> "(" ++pretty t1 ++ ")(" ++ pretty t2 ++ ")"

prettyPrint :: Expr -> IO()
prettyPrint = putStrLn . pretty

showCbvEval, showCbnEval, showNoEval :: Expr -> IO()
showCbvEval = displayEval show cbvStep
showCbnEval = displayEval show cbnStep
showNoEval = displayEval show noStep

prettyCbvEval, prettyCbnEval, prettyNoEval :: Expr -> IO()
prettyCbvEval = displayEval pretty cbvStep
prettyCbnEval = displayEval pretty cbnStep
prettyNoEval = displayEval pretty noStep