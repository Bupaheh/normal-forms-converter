module Expr (Expr(..)) where

type Symb = String

data Expr = Var Symb
          | Expr :* Expr
          | Expr :+ Expr
          | Expr :-> Expr
          | Expr :<-> Expr
          | Not Expr
              deriving Eq
