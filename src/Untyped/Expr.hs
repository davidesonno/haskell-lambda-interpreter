module Untyped.Expr where

import qualified Data.Set as Set

-- Expr ADT --
data Expr = Var String
          | Lam String Expr
          | Appl Expr Expr
    deriving(Show, Eq)

isValue :: Expr -> Bool
isValue (Lam _ _) = True
isValue _ = False

-- FREE VARIABLES --
-- | Computes the free variables in the term @t@.
freeVars :: Expr -> Set.Set String
freeVars t = case t of
    Var x -> Set.singleton x
    Lam x t -> Set.delete x (freeVars t)
    Appl t1 t2 -> Set.union (freeVars t1) (freeVars t2)

-- RENAMING --
freshVar :: Set.Set String -> String -> String
freshVar used x = head $ dropWhile (`Set.member` used) candidates
  where
    candidates = [x ++ replicate n '\'' | n <- [1..]]

-- | Rename the variable @old@ to @new@ in the term @t@.
rename :: String -> String -> Expr -> Expr
rename old new expr = case expr of
    Var y
        | y == old -> Var new
        | otherwise -> Var y
    Lam y body
        | y == old -> Lam new (rename old new body)
        | otherwise -> Lam y (rename old new body)
    Appl t1 t2 -> Appl (rename old new t1) (rename old new t2)

-- SUBSTITUTION --
-- | The variable @x@ gets substituted with the expression @s@ in the term @t@.
sub :: String -> Expr -> Expr -> Expr
sub x s t = case t of
    Var y -> if y == x then s else Var y
    Lam y body
        | y == x -> Lam y body
        | y `Set.member` freeVars s ->
            let y' = freshVar (Set.union (freeVars s) (freeVars body)) y
                body' = rename y y' body
            in Lam y' (sub x s body')
        | otherwise -> Lam y (sub x s body)
    Appl t1 t2 -> Appl (sub x s t1) (sub x s t2)

-- EVALUATION --

-- | Call-by-Value evaluation. Rules: E-APP1 -> E-APP2 -> E-APPABS
-- | Reduce the terms until in the form `Appl value value`, then apply
-- | the second value to the first with substitution.
cbvStep :: Expr -> Maybe Expr
cbvStep expr = case expr of
    Var _ -> Nothing
    Lam _ _ -> Nothing
    Appl t1 t2 ->
        case (isValue t1, isValue t2) of
            (False, _) -> -- E-APP1
                cbvStep t1 >>= \t1' -> Just (Appl t1' t2)

            (True, False) -> -- E-APP2
                cbvStep t2 >>= \t2' -> Just (Appl t1 t2')

            (True, True) -> -- E-APPABS
                case t1 of
                    Lam x t12 -> Just (sub x t2 t12)
                    _ -> Nothing -- just for safety

-- | Call-by-Name evaluation. Rules: E-APP1 -> E-APPABS
-- | If the first value of Appl is a Lam apply it immediately with substitution,
-- | otherwise reduce it.
cbnStep :: Expr -> Maybe Expr
cbnStep e = case e of
    Var _ -> Nothing -- we cannot reduce further a variable
    Lam _ _ -> Nothing -- abstraction are values and cannot be reduced on their own
    Appl t1 t2 ->
        case t1 of
            Lam x t12 -> Just (sub x t2 t12) -- E-APPABS
            _ -> case cbnStep t1 of
                Just t1' -> Just (Appl t1' t2) -- E-APP1
                Nothing -> Nothing
                
-- | full-beta-reduction.
-- fbrStep :: Expr -> Maybe Expr

-- | normal-order. Leftmost, outermost redex first.
noStep :: Expr -> Maybe Expr
noStep e = case e of
    Var _ -> Nothing
    Lam x body -> fmap (Lam x) (noStep body)
    Appl t1 t2 -> 
        case t1 of
            Lam x body -> Just (sub x t2 body) -- if redex, reduce
            _ -> 
                -- if not redex, try to reduce the left tree.
                -- If nothing can be reduced (returned Nothing) go to the right.
                case noStep t1 of
                    Just t1' -> Just (Appl t1' t2)
                    Nothing ->
                        case noStep t2 of
                            Just t2' -> Just (Appl t1 t2')
                            Nothing -> Nothing

-- | Apply complete reduction of the Expr @e@ using the @step@ function to
-- | execute reduction steps. Returns a list with all the steps.
evalSteps :: (Expr -> Maybe Expr) -> Expr -> [Expr]
evalSteps step e = e : case step e of
    Just e' -> evalSteps step e'
    Nothing -> []

-- | Apply complete reduction of the Expr @e@ using the @step@ function to
-- | execute reduction steps. Returns the last step of the reduction.
eval :: (Expr -> Maybe Expr) -> Expr -> Expr
eval step = last . evalSteps step
