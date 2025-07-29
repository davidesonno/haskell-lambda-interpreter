module Typed.Expr where

import Typed.Type

import qualified Data.Set as Set

-- Expr ADT
data Expr = Var String           -- x
          | Lit Int              -- n
          | Add Expr Expr        -- t1 + t2
          | Sub Expr Expr        -- t1 - t2
          | Times Expr Expr      -- t1 * t2
          | Ite Expr Expr Expr   -- if t0 then t1 else t2
          | Pair Expr Expr       -- (t1, t2)
          | Fst Expr             -- fst(t)
          | Snd Expr             -- snd(t)
          | Lam String Type Expr -- \x.t (is putting Type here correct?)
          | Appl Expr Expr       -- (t1 t2)
          | Let String Expr Expr -- let x<=t1 in t2
          | Rec String Expr      -- rec y.(\x.t) (on the book Expr is exactly a Lam ...)
    deriving(Show, Eq)

--  TYPING RULES
type Context = [(String, Type)]

checkIntBinOp :: String -> Context -> Expr -> Expr -> Either String Type
checkIntBinOp opName ctx t1 t2 = do
  typ1 <- typeof ctx t1
  if typ1 /= TInt
    then Left ("The type of the first argument of " ++ opName ++ " is not TInt (found " ++ show typ1 ++ " instead).")
    else do
      typ2 <- typeof ctx t2
      if typ2 /= TInt
        then Left ("The type of the second argument of " ++ opName ++ " is not TInt (found " ++ show typ2 ++ " instead).")
        else Right TInt

typeof :: Context -> Expr -> Either String Type
-- variables
typeof ctx (Var x) = case lookup x ctx of
    Just typ -> Right typ
    Nothing -> Left ("Variable " ++ show x ++ " not in the context (unbound variable).")
-- operations
typeof _ (Lit _) = Right TInt
typeof ctx (Add t1 t2)    = checkIntBinOp "Add" ctx t1 t2
typeof ctx (Sub t1 t2)    = checkIntBinOp "Sub" ctx t1 t2
typeof ctx (Times t1 t2)  = checkIntBinOp "Times" ctx t1 t2
typeof ctx (Ite t0 t1 t2) = do
    typ0 <- typeof ctx t0
    if typ0 /= TInt
        then Left ("The conditional Expr of the Ite is not a TInt (found " ++ show typ0 ++ " instead).")
        else do
            typ1 <- typeof ctx t1
            typ2 <- typeof ctx t2
            if typ1 == typ2
                then Right typ1
                else Left ("The types of the two terms of Ite are different (got " ++ show typ1 ++ " and " ++ show typ2 ++ ").")
-- products
typeof ctx (Pair t1 t2) = case typeof ctx t1 of
    Left err -> Left err
    Right typ1 -> case typeof ctx t2 of
        Left err -> Left err
        Right typ2 -> Right (TPair typ1 typ2)
typeof ctx (Fst p) = case typeof ctx p of
    Left err -> Left err
    Right pTyp -> case pTyp of
        TPair typ1 _ -> Right typ1
typeof ctx (Snd p) = case typeof ctx p of
    Left err -> Left err
    Right pTyp -> case pTyp of
        TPair _ typ2 -> Right typ2
-- functions TODO continue...

freeVars :: Expr -> Set.Set String
freeVars t = case t of
    Lit _        -> Set.empty
    Var x        -> Set.singleton x
    Add t1 t2    -> freeVars t1 `Set.union` freeVars t2
    Sub t1 t2    -> freeVars t1 `Set.union` freeVars t2
    Times t1 t2  -> freeVars t1 `Set.union` freeVars t2
    Ite t0 t1 t2 -> Set.unions [freeVars t0, freeVars t1, freeVars t2]
    Pair t1 t2   -> freeVars t1 `Set.union` freeVars t2
    Fst t        -> freeVars t
    Snd t        -> freeVars t
    Lam x _ t    -> Set.delete x (freeVars t)
    Appl t1 t2   -> freeVars t1 `Set.union` freeVars t2
    Let x t1 t2  -> freeVars t1 `Set.union` Set.delete x (freeVars t2)
    Rec y t -> case t of
        Lam _ _ tBody -> Set.delete y (freeVars tBody)
        _ -> error ("Expr argument of Rec is not a Lam!\n-> Rec " ++ show y ++ " " ++ show t)

