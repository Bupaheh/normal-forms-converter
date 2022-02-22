module Expr (Expr(..)) where

type Symb = String

andSymb = "&"
orSymb = "|"
implSymb = "=>"
eqSymb = "<=>"
notSymb = "~"

notPriority = 4
andPriority = 3
orPriority = 2
implPriority = 1
eqPriority = 0

data Expr = Var Symb
          | Expr :* Expr
          | Expr :+ Expr
          | Expr :-> Expr
          | Expr :<-> Expr
          | Not Expr
              deriving Eq
              

-- instance Show
instance Show Expr where
  showsPrec = myShowsExpr
  
myShows :: String -> ShowS
myShows s suff = s ++ suff
   
myShowsExpr :: Int -> Expr -> ShowS
myShowsExpr _ (Var s) = myShows s

myShowsExpr _ (Not a) = myShows notSymb . myShowsExpr notPriority a

myShowsExpr n (a :* b) = myShowsExprHelper andPriority andSymb n a b
myShowsExpr n (a :+ b) = myShowsExprHelper orPriority orSymb n a b
myShowsExpr n (a :-> b) = myShowsExprHelper implPriority implSymb n a b
myShowsExpr n (a :<-> b) = myShowsExprHelper eqPriority eqSymb n a b

myShowsExprHelper :: Int -> String -> Int -> Expr -> Expr -> ShowS
myShowsExprHelper opPriority s n a b | n <= opPriority = withoutBrackets
                                     | otherwise = myShows "(" . withoutBrackets . myShows ")"
    where str = " " ++ s ++ " " 
          withoutBrackets = myShowsExpr opPriority a . myShows str . myShowsExpr opPriority b

    
-- instance Read

whitespaceRm s = pref ++ suff
    where (pref, suff) = head $ lex s
          
data BinOp = Null | Eq | Impl | Or | And
    deriving (Eq, Show, Ord)
          
getOp :: String -> (BinOp, String)
getOp s = if (res operation == Null) then (res operation, whitespaceRm s) else (res operation, whitespaceRm suff2)
    where (operation, suff2) = head $ lex s
          res op | op == eqSymb     = Eq 
                 | op == implSymb   = Impl
                 | op == orSymb     = Or
                 | op == andSymb    = And
                 | otherwise        = Null
  
binOpToConstr :: BinOp -> Expr -> Expr -> Expr
binOpToConstr op = case op of
                   Eq -> (:<->)
                   Impl -> (:->)
                   Or -> (:+)
                   And -> (:*)
  
applyOp :: [(Expr, BinOp)] -> Expr -> BinOp -> [(Expr, BinOp)]
applyOp st@((ex1, op) : tl) ex2 context | context <= op = applyOp tl (binOpToConstr op ex1 ex2) context
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
