module Typed.Utils where

import Typed.Expr
import Typed.Type

pretty :: Expr -> String
pretty e = case e of
    Var x -> x
    Lit n -> show n
    Add t1 t2 -> "(" ++ pretty t1 ++ " + " ++ pretty t2 ++ ")"
    Sub t1 t2 -> "(" ++ pretty t1 ++ " - " ++ pretty t2 ++ ")"
    Times t1 t2 -> "(" ++ pretty t1 ++ " * " ++ pretty t2 ++ ")"
    Ite t0 t1 t2 -> "if " ++ pretty t0 ++ " then " ++ pretty t1 ++ " else " ++ pretty t2
    Pair t1 t2 -> "(" ++ pretty t1 ++ ", " ++ pretty t2 ++ ")"
    Fst t -> "fst(" ++ pretty t ++ ")"
    Snd t -> "snd(" ++ pretty t ++ ")"
    Lam x ty body -> "\\" ++ x ++ ":" ++ show ty ++ "-> " ++ pretty body
    Appl t1 t2 ->
        let s1 = case t1 of
                Var _ -> pretty t1
                _ -> "(" ++ pretty t1 ++ ")"
            s2 = case t2 of
                Var _ -> pretty t2
                Lit _ -> pretty t2
                _ -> "(" ++ pretty t2 ++ ")"
        in s1 ++ " " ++ s2
    Let x t1 t2 ->
        "let " ++ x ++ " = " ++ pretty t1 ++ " in " ++ pretty t2
    Rec y t@(Lam {}) ->
        "rec " ++ y ++ ".(" ++ pretty t ++ ")"
    Rec y _ ->
        error "Rec argument must be a lambda"

prettyPrint :: Expr -> IO ()
prettyPrint = putStrLn . pretty

typecheck :: Expr -> IO()
typecheck t = do
    putStrLn ("Expr: " ++ pretty t)
    print (typeof [] t)

prettyEvalSteps :: Expr -> IO ()
prettyEvalSteps e = case evalSteps e of
  Left err -> putStrLn err
  Right steps -> mapM_ (\ex -> putStrLn (">> " ++ pretty ex)) steps

printEvalSteps :: Expr -> IO ()
printEvalSteps e = case evalSteps e of
  Left err -> putStrLn err
  Right steps -> mapM_ (\ex -> putStrLn (">> " ++ show ex)) steps
