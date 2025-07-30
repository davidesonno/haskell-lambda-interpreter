module Typed.Type where

data Type = TInt
          | TPair Type Type
          | TFun Type Type
    deriving(Eq)

instance Show Type where
    show TInt = "TInt"
    show (TPair t1 t2) = "(" ++ show t1 ++ ", " ++ show t2 ++ ")"
    show (TFun t1 t2) =
        let left = case t1 of
                TFun _ _ -> "(" ++ show t1 ++ ")"  -- add parentheses if left is a function
                _        -> show t1
            right = show t2
        in left ++ " -> " ++ right
