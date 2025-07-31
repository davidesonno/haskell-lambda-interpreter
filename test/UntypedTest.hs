module UntypedTest where

import Untyped

-- USER VARIABLES --
a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z :: Expr
a = Var "a";b = Var "b";c = Var "c";d = Var "d";e = Var "e";f = Var "f";g = Var "g";h = Var "h";i = Var "i";j = Var "j";k = Var "k";l = Var "l";m = Var "m";n = Var "n";o = Var "o";p = Var "p";q = Var "q";r = Var "r";s = Var "s";t = Var "t";u = Var "u";v = Var "v";w = Var "w";x = Var "x";y = Var "y";z = Var "z";

-- TESTING --

omega = Appl (Lam "x" (Appl x x)) (Lam "x" (Appl x x))

-- types
testBool :: Expr -> Expr 
testBool bool = 
    Appl (Appl (Appl (Lam "l" (Lam "m" (Lam "n" (Appl (Appl l m) n)))) bool) v) w

testTru :: Expr
testTru = cbnEval $ testBool tru

testFls :: Expr
testFls = cbnEval $ testBool fls

-- pairs
testFst_, testSnd_ :: IO()
testFst_ = prettyCbnEval (fst_ (pair x y))
testSnd_ = prettyCbnEval (snd_ (pair x y))

-- church numbers
showNumbers :: Int -> IO()
showNumbers n = mapM_ (putStrLn . pretty . (eval noStep)) $ take n $ iterate scc zero

one, two, three :: Expr
one = scc zero
two = scc one
three = scc two

-- renaming --
testRename :: [Expr]
testRename =
  [ rename "x" "x'" (Var "x"),
    rename "x" "x'" (Var "y"),
    rename "x" "x'" (Lam "x" (Var "x")),
    rename "x" "x'" (Lam "y" (Lam "x" (Appl (Var "x") (Var "y")))),
    rename "x" "x'" (Lam "y" (Appl (Var "x") (Var "y"))),
    rename "x" "x'" (Appl (Var "x") (Var "x")),
    rename "z" "z'" (Lam "x" (Appl (Var "z") (Lam "z" (Var "z"))))
  ]

expectedRename :: [Expr]
expectedRename =
  [ Var "x'",
    Var "y",
    Lam "x'" (Var "x'"),
    Lam "y" (Lam "x'" (Appl (Var "x'") (Var "y"))),
    Lam "y" (Appl (Var "x'") (Var "y")),
    Appl (Var "x'") (Var "x'"),
    Lam "x" (Appl (Var "z'") (Lam "z'" (Var "z'")))
  ]

renameOK :: Bool
renameOK = testRename == expectedRename

-- free variables -- 
fv1 = freeVars (Var "x")  
    -- → Set.fromList ["x"]

fv2 = freeVars (Lam "x" (Var "x")) 
    -- → Set.fromList []

fv3 = freeVars (Lam "x" (Var "y")) 
    -- → Set.fromList ["y"]

fv4 = freeVars (Appl (Var "x") (Var "y")) 
    -- → Set.fromList ["x", "y"]

fv5 = freeVars (Lam "x" (Appl (Var "x") (Var "y"))) 
    -- → Set.fromList ["y"]

fv6 = freeVars (Lam "x" (Lam "y" (Appl (Var "x") (Var "z")))) 
    -- → Set.fromList ["z"]

-- subtitutions --
sub1 = sub "x" (Var "z") (Var "x")
    -- → Var "z"

sub2 = sub "x" (Var "z") (Var "y")
    -- → Var "y"

sub3 = sub "x" (Var "z") (Appl (Var "x") (Var "y"))
    -- → Appl (Var "z") (Var "y")
    
sub4 = sub "y" (Var "w") (Appl (Var "x") (Var "y"))
    -- → Appl (Var "x") (Var "w")
    
sub5 = sub "x" (Var "z") (Lam "y" (Var "x"))
    -- → Lam "y" (Var "z")  -- x is free inside
    
sub6 = sub "x" (Var "z") (Lam "x" (Var "x"))
    -- → Lam "x" (Var "x")  -- x is bound: do not substitute!
    
sub7 = sub "x" (Var "z") (Appl (Lam "y" (Var "x")) (Var "x"))
    -- → Appl (Lam "y" (Var "z")) (Var "z")
    
sub8 = sub "x" (Var "z") (Lam "x" (Appl (Var "x") (Var "y")))
    -- → Lam "x" (Appl (Var "x") (Var "y"))  -- x is bound, no substitution
    
sub9 = sub "x" (Var "y") (Lam "y" (Appl (Var "x") (Var "y")))
    -- → Lam "y'" (Appl (Var "y") (Var "y'"))

-- SAMPLE TERMS --

-- (λx. x) y
t1,t2,t3 :: Expr
t1 = Appl (Lam "x" x) y

t2 = sub "x" (Appl y z) (Lam "y" (Appl x y))

t3 = Appl (Lam "x" (Var "x")) (Appl (Lam "y" (Var "y")) (Var "z"))

t4 = Appl (Lam "x" (Var "x")) (Appl (Lam "x" (Var "x")) (Lam "z" (Appl (Lam "x" (Var "x")) (Var "z"))))

-- (λx.x) ((λx.x) (λz. (λx.x) z)) = id (id (λz. id z))
t5 = id_ ( id_ (Lam "z" (id_ z)))

t6 = (fst_ (pair x y))