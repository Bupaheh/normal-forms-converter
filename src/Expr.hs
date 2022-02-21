module Expr (Expr(..)) where

type Symb = String

data Expr = Var Symb
          | Expr :* Expr
          | Expr :+ Expr
          | Expr :-> Expr
          | Expr :<-> Expr
          | Not Expr
              deriving Eq
              

-- instance Show
instance Show Expr where
  showsPrec _ = myShowsExpr 0
  
myShows :: String -> ShowS
myShows s suff = s ++ suff
   
myShowsExpr :: Int -> Expr -> ShowS
myShowsExpr _ (Var s) = myShows s

myShowsExpr _ (Not a) = myShows "¬ " . myShowsExpr opPriority a
    where opPriority = 4

myShowsExpr n (a :* b) = myShowsExprHelper 3 " ∧ " n a b
myShowsExpr n (a :+ b) = myShowsExprHelper 2 " ∨ " n a b
myShowsExpr n (a :-> b) = myShowsExprHelper 1 " -> " n a b
myShowsExpr n (a :<-> b) = myShowsExprHelper 0 " <-> " n a b

myShowsExprHelper :: Int -> String -> Int -> Expr -> Expr -> ShowS
myShowsExprHelper opPriority s n a b | n <= opPriority = withoutBrackets
                                     | otherwise = myShows "(" . withoutBrackets . myShows ")"
    where withoutBrackets = myShowsExpr opPriority a . myShows s . myShowsExpr opPriority b
