module Expr (Expr(..)) where

type Symb = String

andSymb = "&"
orSymb = "|"
implSymb = "=>"
eqSymb = "<=>"
notSymb = "~"

notPr = 5
andPr = 4
orPr = 3
implPrR = 2
implPr = 1
eqPr = 0

infixl 7 :&
infixl 6 :|
infixl 5 :=>
infixl 4 :<=>

data Expr = Var Symb
          | Expr :& Expr
          | Expr :| Expr
          | Expr :=> Expr
          | Expr :<=> Expr
          | Not Expr
              deriving Eq
              

-- instance Show
instance Show Expr where
  showsPrec = myShowsExpr
  
myShows :: String -> ShowS
myShows s suff = s ++ suff
   
myShowsExpr :: Int -> Expr -> ShowS
myShowsExpr _ (Var s) = myShows s

myShowsExpr _ (Not a) = myShows notSymb . myShowsExpr notPr a

myShowsExpr context (a :& b) = myShowsExprHelper andPr andSymb context (andPr, andPr) a b
myShowsExpr context (a :| b) = myShowsExprHelper orPr orSymb context (orPr, orPr) a b
myShowsExpr context (a :=> b@(c :=> d)) = myShowsExprHelper implPr implSymb context (implPr, implPrR) a b
myShowsExpr context (a :=> b) = myShowsExprHelper implPr implSymb context (implPr, implPr) a b
myShowsExpr context (a :<=> b) = myShowsExprHelper eqPr eqSymb context (eqPr, eqPr) a b

myShowsExprHelper :: Int -> String -> Int -> (Int, Int) -> Expr -> Expr -> ShowS
myShowsExprHelper opPr opSymb context (contextL, contextR) a b | context <= opPr    = withoutBrackets 
                                                               | otherwise          = myShows "(" . withoutBrackets . myShows ")"
    where withoutBrackets = myShowsExpr contextL a . myShows opSymb' . myShowsExpr contextR b
          opSymb' = " " ++ opSymb ++ " " 

    
-- instance Read

whitespaceRm s = pref ++ suff
    where (pref, suff) = head $ lex s
          
data BinOp = Null | Eq | Impl | Or | And
    deriving (Eq, Show, Ord)
          
getOp :: String -> (BinOp, String)
getOp s = if (res' == Null) then (res', whitespaceRm s) else (res', whitespaceRm suff2)
    where (operation, suff2) = head $ lex s
          res' = res operation
          res op | op == eqSymb     = Eq 
                 | op == implSymb   = Impl
                 | op == orSymb     = Or
                 | op == andSymb    = And
                 | otherwise        = Null
  
binOpToConstr :: BinOp -> Expr -> Expr -> Expr
binOpToConstr op = case op of
                   Eq -> (:<=>)
                   Impl -> (:=>)
                   Or -> (:|)
                   And -> (:&)
  
applyOp :: [(Expr, BinOp)] -> Expr -> BinOp -> [(Expr, BinOp)]
applyOp [] ex context = [(ex, context)]
applyOp st@((ex1, op) : tl) ex2 context | context <= op = applyOp tl (binOpToConstr op ex1 ex2) context
                                        | otherwise = (ex2, context) : st
    
instance Read Expr where
  readsPrec _ = myReadExpr []          
          
myReadExpr :: [(Expr, BinOp)] -> ReadS Expr
myReadExpr [(ex, Null)] s = [(ex, s)]

myReadExpr st ('(' : s) = myReadExpr st' suff''
    where (ex, ')' : suff) = head $ myReadExpr [] s
          suff' = whitespaceRm suff
          (op, suff'') = getOp suff'
          st' = applyOp st ex op
          
myReadExpr st str@('~' : s) = case next of
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
getNotConstr ('~' : s) = (Not . f, str)
    where (f, str) = getNotConstr $ whitespaceRm s
    
getNotConstr s = (id, s)
