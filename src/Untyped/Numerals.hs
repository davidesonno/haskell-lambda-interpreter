module Untyped.Numerals where

import Untyped.Expr
import Untyped.Bool

zero :: Expr
zero = Lam "s" (Lam "z" (Var "z"))

-- successor
sccExpr :: Expr
sccExpr = Lam "n" (Lam "s" (Lam "z" (Appl (Var "s") (Appl (Appl (Var "n") (Var "s")) (Var "z")))))

scc :: Expr -> Expr
scc = Appl sccExpr

-- addition
plusExpr :: Expr
plusExpr = Lam "m" (Lam "n" (Lam "s" (Lam "z" (Appl (Appl (Var "m") (Var "s")) (Appl (Appl (Var "n") (Var "s")) (Var "z"))))))

plus :: Expr -> Expr -> Expr
plus m n = Appl (Appl plusExpr m) n

-- multiplication
-- λm. λn. λs. λz. m (n s) z (the one in the book using plus did not work).
timesExpr :: Expr
timesExpr = Lam "m" (Lam "n" (Lam "s" (Lam "z" (Appl (Appl (Var "m") (Appl (Var "n") (Var "s"))) (Var "z")))))

times :: Expr -> Expr -> Expr
times m n = Appl (Appl timesExpr m) n

-- iszero
iszeroExpr :: Expr
iszeroExpr = Lam "m" (Appl (Appl (Var "m") (Lam "x" fls)) tru)

iszero :: Expr -> Expr
iszero = Appl iszeroExpr