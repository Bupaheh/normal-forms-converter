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

    
-- instance Read

whitespaceRm s = pref ++ suff
    where (pref, suff) = head $ lex s
          
data BinOp = Null | Eq | Impl | Or | And
    deriving (Eq, Show, Ord)
          
getOp :: String -> (BinOp, String)
getOp s = if (res == Null) then (res, s) else (res, whitespaceRm suff2)
    where (operation, suff2) = head $ lex s
          res = case operation of 
                     "<->" -> Eq
                     "->" -> Impl
                     "+" -> Or
                     "*" -> And
                     _ -> Null
  
toConstr :: BinOp -> Expr -> Expr -> Expr
toConstr op = case op of
                   Eq -> (:<->)
                   Impl -> (:->)
                   Or -> (:+)
                   And -> (:*)
  
applyOp :: [(Expr, BinOp)] -> Expr -> BinOp -> [(Expr, BinOp)]
applyOp st@((ex1, op) : tl) ex2 context | context <= op = applyOp tl (toConstr op ex1 ex2) context
                                        | otherwise = (ex2, context) : st
                                        
applyOp [] ex context = [(ex, context)]
    
instance Read Expr where
  readsPrec _ = myReadExpr []          
          
myReadExpr :: [(Expr, BinOp)] -> ReadS Expr
myReadExpr [(ex, Null)] s = [(ex, s)]

myReadExpr st ('(' : s) = myReadExpr st' suff''
    where (ex, ')' : suff) = head $ myReadExpr [] s
          suff' = whitespaceRm suff
          (op, suff'') = getOp suff'
          st' = applyOp st ex op
          
myReadExpr st str@('!' : s) = case next of
                               '(' : _ -> myReadExpr (applyOp st (constr ex1) op) suff'
                               _ -> myReadExpr (applyOp st ex2 op2) suff2'
    where (constr, next) = getNotConstr str
          (ex1, ')' : suff) = head $ myReadExpr [] $ tail next
          (op, suff') = getOp suff
          (name, suff2) = head $ lex next
          ex2 = constr $ Var name
          (op2, suff2') = getOp suff2
          
myReadExpr st s = myReadExpr (applyOp st ex op) suff''
    where (name, suff) = head $ lex s
          suff' = whitespaceRm suff
          ex = Var name
          (op, suff'') = getOp suff' 
          
getNotConstr :: String -> (Expr -> Expr, String)
getNotConstr ('!' : s) = (Not . f, str)
    where (f, str) = getNotConstr $ whitespaceRm s
    
getNotConstr s = (id, s)
