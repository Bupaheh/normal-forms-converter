module Lib (toNNF, toDNF, toCNF, readExpr) where

import Expr(Expr(..))

toBasis :: Expr -> Expr
toBasis (Var a) = Var a
toBasis (Not a) = Not $ toBasis a
toBasis (a :& b) = toBasis a :& toBasis b
toBasis (a :| b) = toBasis a :| toBasis b
toBasis (a :=> b) = (Not $ toBasis a) :| toBasis b
toBasis (a :<=> b) = (Not a' :| b') :& (Not b' :| a')
    where a' = toBasis a
          b' = toBasis b

-- applies De Morgan's Laws and removes double Not
dml :: Expr -> Expr
dml (a :=> b) = undefined
dml (a :<=> b) = undefined
dml (Var a) = Var a
dml (Not (Not a)) = dml a
dml (Not (a :| b)) = dml (Not a) :& dml (Not b)
dml (Not (a :& b)) = dml (Not a) :| dml (Not b)
dml (Not a) = Not $ dml a
dml (a :& b) = dml a :& dml b
dml (a :| b) = dml a :| dml b

distr :: Expr -> Expr
distr (a :=> b) = undefined
distr (a :<=> b) = undefined
distr (a :| b) = distr a :| distr b
distr (a :& b) = case expr of
                      d :& (e :| f) -> distr (d :& e) :| distr (d :& f)
                      (d :| e) :& f -> distr (d :& f) :| distr (e :& f)
                      _ -> expr
    where a' = distr a
          b' = distr b
          expr = a' :& b'
distr a = a

readExpr :: String -> Expr
readExpr = read

toNNF :: Expr -> Expr
toNNF = dml . toBasis

toDNF :: Expr -> Expr
toDNF = distr . toNNF

toCNF :: Expr -> Expr
toCNF = dml . Not . toDNF . Not
