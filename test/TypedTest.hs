module TypedTest where

import Typed

-- TYPECHECKS
t1, t2, t3, t4, t5 :: Expr

t1 = Lam "x" TInt (Add (Var "x") (Lit 1)) 
-- Type: TInt -> TInt

t2 = Pair (Lit 3) (Lit 5) 
-- Type: TPair TInt TInt

t3 = Lam "p" (TPair TInt TInt) (Fst (Var "p")) 
-- Type: (TInt, TInt) -> TInt

t4 = Rec "f" (Lam "x" TInt (
       Ite (Sub (Var "x") (Lit 1))
           (Times (Var "x") (Appl (Var "f") (Sub (Var "x") (Lit 1))))
           (Lit 1)
     ))
-- Type: TInt -> TInt

t5 = Let "x" (Lit 5) (Add (Var "x") (Lit 2)) 
-- Type: TInt

-- t6: Extract second element from a pair
t6 = Lam "p" (TPair TInt TInt) (Snd (Var "p")) 
-- Type: (TInt, TInt) -> TInt

-- t7: Let-binding with a pair, then extracting fst
t7 = Let "pair" (Pair (Lit 7) (Lit 8)) (Fst (Var "pair")) 
-- Type: TInt

-- t8: Nested let-bindings
t8 = Let "x" (Lit 10) (
       Let "y" (Lit 20) (
         Add (Var "x") (Var "y")
       )
     )
-- Type: TInt

-- t9: Lambda that returns a pair of integers
t9 = Lam "x" TInt (Pair (Var "x") (Lit 42)) 
-- Type: TInt -> (TInt, TInt)

-- t10: Recursive function that returns a pair
-- rec g. \x ->
--   if x - 1 then (x, g(x - 1)) else (0, 0)
t10 = Rec "g" (Lam "x" TInt (
        Ite (Sub (Var "x") (Lit 1))
            (Pair (Var "x") (Appl (Var "g") (Sub (Var "x") (Lit 1))))
            (Pair (Lit 0) (Lit 0))
      ))
-- Type: TInt -> (TInt, TInt)

-- t11: Function applying another function twice
t11 = Lam "f" (TFun TInt TInt) (Lam "x" TInt (Appl (Var "f") (Appl (Var "f") (Var "x"))))
-- Type: (TInt -> TInt) -> TInt -> TInt

-- t12: if-then-else with boolean-like Int condition (nonzero = true)
t12 = Lam "x" TInt (
        Ite (Var "x") (Lit 1) (Lit 0)
      )
-- Type: TInt -> TInt

-- FAILING TYPECHECKS
-- f1: Adding an Int and a function (type error)
f1 = Add (Lit 3) (Lam "x" TInt (Var "x"))  
-- Error: trying to add TInt and (TInt -> TInt)

-- f2: Applying an Int as if it were a function
f2 = Appl (Lit 5) (Lit 2)
-- Error: Lit 5 is TInt, not a function

-- f3: Using fst on an Int (not a pair)
f3 = Fst (Lit 42)
-- Error: fst expects a pair, got TInt

-- f4: Let-binding with type mismatch in body
f4 = Let "x" (Lit 1) (Add (Var "x") (Lam "y" TInt (Var "y")))
-- Error: Add expects two Ints, got TInt and (TInt -> TInt)

-- f5: Using snd on an Int
f5 = Snd (Lit 7)
-- Error: snd expects a pair, got TInt

-- f6: Ite condition not Int (using a pair instead)
f6 = Ite (Pair (Lit 1) (Lit 2)) (Lit 1) (Lit 0)
-- Error: condition of Ite should be Int

-- f7: Lambda argument annotation mismatch
-- Trying to apply function with incorrect argument type
f7 = Appl (Lam "x" TInt (Add (Var "x") (Lit 1))) (Pair (Lit 1) (Lit 2))
-- Error: applied argument type (TPair TInt TInt) does not match parameter type TInt
