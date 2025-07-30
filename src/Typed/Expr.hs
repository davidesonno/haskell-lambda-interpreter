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
    typ2 <- typeof ctx t2
    if (typ1, typ2) == (TInt, TInt)
        then Right TInt
        else Left ("Type error: Trying to apply " ++ opName ++ " between " ++ show typ1 ++ " and " ++ show typ2 ++ ".")

typeof :: Context -> Expr -> Either String Type
-- variables
typeof ctx (Var x) = case lookup x ctx of
    Just typ -> Right typ
    Nothing -> Left ("Type error: Variable " ++ show x ++ " not in the context (unbound variable).")
-- operations
typeof _ (Lit _) = Right TInt

typeof ctx (Add t1 t2)    = checkIntBinOp "Add" ctx t1 t2
typeof ctx (Sub t1 t2)    = checkIntBinOp "Sub" ctx t1 t2
typeof ctx (Times t1 t2)  = checkIntBinOp "Times" ctx t1 t2

typeof ctx (Ite t0 t1 t2) = do
    typ0 <- typeof ctx t0
    if typ0 /= TInt
        then Left ("Type error: The conditional Expr of the Ite is not a TInt (found " ++ show typ0 ++ " instead).")
        else do
            typ1 <- typeof ctx t1
            typ2 <- typeof ctx t2
            if typ1 == typ2
                then Right typ1
                else Left ("Type error: The types of the two terms of Ite are different (got " ++ show typ1 ++ " and " ++ show typ2 ++ ").")
-- products
typeof ctx (Pair t1 t2) = do
    typ1 <- typeof ctx t1
    typ2 <- typeof ctx t2
    Right (TPair typ1 typ2)

typeof ctx (Fst t) = do
    typ <- typeof ctx t
    case typ of
        TPair typ1 _ -> Right typ1
        _ -> Left ("Type error: Fst expected a pair, got " ++ show typ)

typeof ctx (Snd t) = do
    typ <- typeof ctx t
    case typ of
        TPair _ typ2 -> Right typ2
        _ -> Left ("Type error: Snd expected a pair, got " ++ show typ)
-- functions 
typeof ctx (Lam x typ1 t) = do
    typ2 <- typeof ((x, typ1) : ctx) t
    Right (TFun typ1 typ2)

typeof ctx (Appl t1 t2) = do
    funType <- typeof ctx t1
    case funType of
        TFun typ11 typ12 -> do
            argType <- typeof ctx t2
            if typ11 /= argType
                then Left ("Type error: The first term of the Appl has type " ++ show typ11 ++ " but the applied argument has type " ++ show argType ++ ".")
                else Right typ12
        typ -> Left ("Type error: The type of the first term of the Appl is not TFun (got " ++ show typ ++ " instead).")
-- let
typeof ctx (Let x t1 t2) = do
    typ1 <- typeof ctx t1
    typeof ((x, typ1) : ctx) t2
-- rec
typeof ctx (Rec y t) = case t of
    Lam x argTyp body -> do
        let assumedRetTyp = TInt
            assumedType = TFun argTyp assumedRetTyp
        bodyTyp <- typeof ((x, argTyp):(y, assumedType):ctx) body
        let finalType = TFun argTyp bodyTyp
        if assumedType == finalType
            then return finalType
            else Left $ "Type mismatch in recursive function: assumed " ++ show assumedType ++
                        ", but actual " ++ show finalType
    _ -> error ("Expr argument of Rec is not a Lam!\n-> Rec " ++ show y ++ " " ++ show t)

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

