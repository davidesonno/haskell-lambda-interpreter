module TypedTest where

import Typed

-- USER VARIABLES --
a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v, w, x, y, z :: Expr
a = Var "a"
b = Var "b"
c = Var "c"
d = Var "d"
e = Var "e"
f = Var "f"
g = Var "g"
h = Var "h"
i = Var "i"
j = Var "j"
k = Var "k"
l = Var "l"
m = Var "m"
n = Var "n"
o = Var "o"
p = Var "p"
q = Var "q"
r = Var "r"
s = Var "s"
t = Var "t"
u = Var "u"
v = Var "v"
w = Var "w"
x = Var "x"
y = Var "y"
z = Var "z"

-- TYPECHECKS

t1 = Lam "x" TInt (Add (Var "x") (Lit 1))

-- Type: TInt -> TInt

t2 = Pair (Lit 3) (Lit 5)

-- Type: TPair TInt TInt

t3 = Lam "p" (TPair TInt TInt) (Fst (Var "p"))

-- Type: (TInt, TInt) -> TInt

t4 =
  Rec
    "f"
    ( Lam
        "x"
        TInt
        ( Ite
            (Sub (Var "x") (Lit 1))
            (Times (Var "x") (Appl (Var "f") (Sub (Var "x") (Lit 1))))
            (Lit 1)
        )
    )

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
t8 =
  Let
    "x"
    (Lit 10)
    ( Let
        "y"
        (Lit 20)
        ( Add (Var "x") (Var "y")
        )
    )

-- Type: TInt

-- t9: Lambda that returns a pair of integers
t9 = Lam "x" TInt (Pair (Var "x") (Lit 42))

-- Type: TInt -> (TInt, TInt)

-- t10: Function applying another function twice
t10 = Lam "f" (TFun TInt TInt) (Lam "x" TInt (Appl (Var "f") (Appl (Var "f") (Var "x"))))

-- Type: (TInt -> TInt) -> TInt -> TInt

t11 = Appl (Appl t10 (Lam "y" TInt (Add y (Lit 1)))) (Lit 3)

-- Type: TInt

-- t12: if-then-else with boolean-like Int condition (nonzero = true)
t12 =
  Lam
    "x"
    TInt
    ( Ite (Var "x") (Lit 1) (Lit 0)
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

-- Identity function applied to 5: (\x:Int. x) 5
e1 :: Expr
e1 = Appl (Lam "x" TInt (Var "x")) (Lit 5)

-- Let binding: let x = 10 in x + 1
e2 :: Expr
e2 = Let "x" (Lit 10) (Add (Var "x") (Lit 1))

-- Recursive factorial function applied to 3:
-- rec f. \x:Int. if x - 1 then x * f(x - 1) else 1  applied to 3
e3 :: Expr
e3 = Appl t4 (Lit 3)

fact :: Int -> Expr
fact n = Appl t4 (Lit n)

fibExpr :: Expr
fibExpr =
  Rec
    "f"
    ( Lam
        "x"
        TInt
        ( Ite
            (Var "x")  -- if x /= 0 then ...
            ( Ite
                (Sub (Var "x") (Lit 1))  -- if x - 1 /= 0 → x > 1
                ( Add
                    (Appl (Var "f") (Sub (Var "x") (Lit 1)))
                    (Appl (Var "f") (Sub (Var "x") (Lit 2)))
                )
                (Lit 1)  -- x == 1
            )
            (Lit 1) -- x == 0
        )
    )


fib :: Int -> Expr
fib n = Appl fibExpr (Lit n)