module Untyped.Pair where

import Untyped.Expr
import Untyped.Bool

pairExp :: Expr
pairExp = Lam "f" (Lam  "s" (Lam "b" (Appl (Appl (Var "b") (Var "f")) (Var "s"))))

fstExpr :: Expr
fstExpr = Lam "p" (Appl (Var "p") tru)

sndExpr :: Expr
sndExpr = Lam "p" (Appl (Var "p") fls)

pair :: Expr -> Expr -> Expr
pair v w = Appl (Appl pairExp v) w

fst_ :: Expr -> Expr
fst_ p = Appl fstExpr p

snd_ :: Expr -> Expr
snd_ p = Appl sndExpr p