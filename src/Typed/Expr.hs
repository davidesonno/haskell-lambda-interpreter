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
            else Left $ "Type error: Type mismatch in recursive function: assumed " ++ show assumedType ++
                        ", but actual " ++ show finalType
    _ -> error ("Expr argument of Rec is not a Lam!\n-> Rec " ++ show y ++ " " ++ show t)

-- FREE VARIABLES
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
        Lam _ _ body -> Set.delete y (freeVars body)
        _ -> error ("Expr argument of Rec is not a Lam!\n-> Rec " ++ show y ++ " " ++ show t)

-- EVALUATION
isCanonicalForm :: Expr -> Bool
isCanonicalForm (Lit _) = True
isCanonicalForm (Pair c1 c2) = isCanonicalForm c1 && isCanonicalForm c2
isCanonicalForm (Lam x typ t) = Set.null (freeVars (Lam x typ t))
isCanonicalForm _ = False

-- RENAMING --
freshVar :: Set.Set String -> String -> String
freshVar used x = head $ dropWhile (`Set.member` used) candidates
  where
    candidates = [x ++ replicate n '\'' | n <- [1..]]

-- | Rename the variable @old@ to @new@ in the term @t@.
rename :: String -> String -> Expr -> Expr
rename old new expr = case expr of
    Var x
        | x == old -> Var new
        | otherwise -> Var x
    Lit n -> Lit n
    Add t1 t2 -> Add (rename old new t1) (rename old new t2)
    Sub t1 t2 -> Sub (rename old new t1) (rename old new t2)
    Times t1 t2 -> Times (rename old new t1) (rename old new t2)
    Ite t0 t1 t2 -> Ite (rename old new t0) (rename old new t1) (rename old new t2)
    Pair t1 t2 -> Pair (rename old new t1) (rename old new t2)
    Fst p -> Fst (rename old new p)
    Snd p -> Snd (rename old new p)
    Lam x typ body
        | x == old -> Lam new typ (rename old new body)
        | otherwise -> Lam x typ (rename old new body)
    Appl t1 t2 -> Appl (rename old new t1) (rename old new t2)
    Let x t1 t2
        | x == old -> Let new (rename old new t1) (rename old new t2)
        | otherwise -> Let x (rename old new t1) (rename old new t2)
    Rec y t -> case t of
        Lam y2 typ2 body
            | y == old  -> Rec new (Lam y2 typ2 (rename old new body))
            | y2 == old -> Rec y (Lam new typ2 (rename old new body))
            | otherwise -> Rec y (Lam y2 typ2 (rename old new body))
        _ -> error ("Expr argument of Rec is not a Lam!\n-> Rec " ++ show y ++ " " ++ show t)

-- SUBSTITUTION --
-- | The variable @x@ gets substituted with the expression @s@ in the term @t@.
sub :: String -> Expr -> Expr
    -> Expr
sub x s t = case t of
    Var y -> if x==y then s else t
    Lit _ -> t
    Add t1 t2 -> Add (sub x s t1) (sub x s t2)
    Sub t1 t2 -> Sub (sub x s t1) (sub x s t2)
    Times t1 t2 -> Times (sub x s t1) (sub x s t2)
    Ite t0 t1 t2 -> Ite (sub x s t0) (sub x s t1) (sub x s t2)
    Pair t1 t2 -> Pair (sub x s t1) (sub x s t2)
    Fst (Pair t1 t2) -> Fst (Pair (sub x s t1) (sub x s t2))
    Fst p -> Fst (sub x s p)
    Snd (Pair t1 t2) -> Snd (Pair (sub x s t1) (sub x s t2))
    Snd p -> Snd (sub x s p)
    Lam y typ t -- here variables are bound
        | y == x -> Lam y typ t
        | y `Set.member` freeVars s ->
            let y' = freshVar (Set.union (freeVars s) (freeVars t)) y
                t' = rename y y' t
            in Lam y' typ (sub x s t')
        | otherwise -> Lam y typ (sub x s t)
    Appl t1 t2 -> Appl (sub x s t1) (sub x s t2)
    Let y t1 t2 -> -- here variables are bound
        let t1' = sub x s t1 -- always sub the first term
        in if y == x
            then Let y t1' t2  -- dont sub t2
            else if y `Set.member` freeVars s -- rename
                then
                    let y' = freshVar (freeVars s `Set.union` freeVars t2 `Set.union` Set.singleton x) y
                        t2' = sub y (Var y') t2
                    in Let y' t1' (sub x s t2')
                else
                    Let y t1' (sub x s t2) -- sub t2
    Rec y1 t -> case t of
        Lam y2 typ2 body ->
            if x == y1 || x == y2 then
                Rec y1 (Lam y2 typ2 body)  -- dont sub
            else
                let fvS = freeVars s
                    fvBody = freeVars body
                    avoid = fvS `Set.union` fvBody `Set.union` Set.singleton x
                    (y1', body1) =
                        if y1 `Set.member` fvS -- sub y1
                        then let y1' = freshVar avoid y1
                             in (y1', sub y1 (Var y1') body)
                        else (y1, body)
                    (y2', body2) =
                        if y2 `Set.member` fvS -- sub y2
                        then let y2' = freshVar (avoid `Set.union` Set.singleton y1') y2
                             in (y2', sub y2 (Var y2') body1)
                        else (y2, body1)
                    body' = sub x s body2
                in Rec y1' (Lam y2' typ2 body')
        _ -> error ("Expr argument of Rec is not a Lam!\n-> Rec " ++ show y1 ++ " " ++ show t)

-- EVALUATION
-- | leftmost reduction first
evalStep :: Expr -> Maybe Expr
evalStep (Var _) = Nothing
evalStep (Lit n) = Nothing

-- operations
evalStep (Add (Lit n1) (Lit n2)) = Just (Lit (n1 + n2))
evalStep (Add t1 t2) -- reduce first left, if it is a canonical form go right.
    | not (isCanonicalForm t1) = fmap (`Add` t2) (evalStep t1)
    | not (isCanonicalForm t2) = fmap (Add t1) (evalStep t2)
    | otherwise = Nothing -- both reduced but not `Lit`s

evalStep (Sub (Lit n1) (Lit n2)) = Just (Lit (n1 - n2))
evalStep (Sub t1 t2)
    | not (isCanonicalForm t1) = fmap (`Sub` t2) (evalStep t1)
    | not (isCanonicalForm t2) = fmap (Sub t1) (evalStep t2)
    | otherwise = Nothing

evalStep (Times (Lit n1) (Lit n2)) = Just (Lit (n1 * n2))
evalStep (Times t1 t2)
    | not (isCanonicalForm t1) = fmap (`Times` t2) (evalStep t1)
    | not (isCanonicalForm t2) = fmap (Times t1) (evalStep t2)
    | otherwise = Nothing

evalStep (Ite (Lit 0) _ t2) = Just t2
evalStep (Ite (Lit _) t1  _) = Just t1
evalStep (Ite t0 t1 t2) = fmap (\t0' -> Ite t0' t1 t2) (evalStep t0)

-- product
evalStep (Pair t1 t2)
    | not (isCanonicalForm t1) = fmap (`Pair` t2) (evalStep t1)
    | not (isCanonicalForm t2) = fmap (Pair t1) (evalStep t2)
    | otherwise = Nothing

evalStep (Fst (Pair t1 t2))
    | isCanonicalForm t1 && isCanonicalForm t2 = Just t1
evalStep (Fst p) = fmap Fst (evalStep p)

evalStep (Snd (Pair t1 t2))
    | isCanonicalForm t1 && isCanonicalForm t2 = Just t1
evalStep (Snd p) = fmap Snd (evalStep p)

-- function
evalStep (Appl (Lam x typ t1) t2)
    | isCanonicalForm t2 = Just (sub x t2 t1)
    | otherwise = fmap (Appl (Lam x typ t1)) (evalStep t2)
evalStep (Appl t1 t2)
    | not (isCanonicalForm t1) = fmap (`Appl` t2) (evalStep t1)
    | not (isCanonicalForm t2) = fmap (Appl t1) (evalStep t2)
    | otherwise = Nothing

-- let
evalStep (Let x t1 t2)
    | isCanonicalForm t1 = Just (sub x t1 t2)
    | otherwise = fmap (\t1' -> Let x t1' t2) (evalStep t1)

-- rec
-- evalStep (Rec y (Lam x typ t)) = Just (Lam x typ (sub y (Rec y (Lam x typ t)) t))
-- or
evalStep (Rec y (Lam x typ t)) = Just (sub y (Rec y (Lam x typ t)) (Lam x typ t))

evalStep _ = Nothing

-- | Apply complete reduction of the Expr @e@ only if it typechecks.
-- Returns Left with the error message or Right with all steps.
evalSteps :: Expr -> Either String [Expr]
evalSteps e = do
  _ <- typeof [] e          -- type check with empty context
  return (evalSteps' e)     -- call the original evalSteps internally

-- | Apply reduction without typechecking
evalSteps' :: Expr -> [Expr]
evalSteps' e = e : maybe [] evalSteps' (evalStep e)

-- | Apply complete reduction of the Expr @e@ only if it typechecks.
-- Returns Left with the error message or Right with the final expression.
eval :: Expr -> Either String Expr
eval e = do
  _ <- typeof [] e
  return (last (evalSteps' e))
