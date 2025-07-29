module Untyped.Bool where

import Untyped.Expr

tru :: Expr
tru = Lam "t" (Lam "f" (Var "t"))

fls :: Expr
fls = Lam "t" (Lam "f" (Var "f"))

andExpr :: Expr
andExpr = Lam "b" (Lam "c" (Appl (Appl (Var "b") (Var "c")) fls))

and_ :: Expr -> Expr -> Expr
and_ b1 b2 = Appl (Appl andExpr b1) b2
