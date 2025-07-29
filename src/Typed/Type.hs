module Typed.Type where

data Type = TInt
          | TPair Type Type
          | TFun Type Type
    deriving(Show, Eq)