module Lib
    ( someFunc
    ) where

import Expr(Expr(..))

t1 = read "(! !(a + !b) * c) -> ((a + c) * !d)" :: Expr

toBasis :: Expr -> Expr
toBasis (Var a) = Var a
toBasis (Not a) = Not $ toBasis a
toBasis (a :* b) = toBasis a :* (toBasis b)
toBasis (a :+ b) = (toBasis a) :+ (toBasis b)
toBasis (a :-> b) = (Not $ toBasis a) :+ (toBasis b)
toBasis (a :<-> b) = (Not a' :+ b') :* (Not b' :+ a')
    where a' = toBasis a
          b' = toBasis b
          
rmDoubleNot :: Expr -> Expr
rmDoubleNot (Var a) = Var a
rmDoubleNot (a :+ b) = rmDoubleNot a :+ rmDoubleNot b
rmDoubleNot (a :* b) = rmDoubleNot a :* rmDoubleNot b
rmDoubleNot (a :-> b) = rmDoubleNot a :-> rmDoubleNot b
rmDoubleNot (a :<-> b) = rmDoubleNot a :<-> rmDoubleNot b
rmDoubleNot (Not (Not a)) = rmDoubleNot a
rmDoubleNot (Not a) = Not $ rmDoubleNot a

dml :: Expr -> Expr
dml (Var a) = Var a
dml (Not (a :+ b)) = dml (Not a) :* dml (Not b)
dml (Not (a :* b)) = dml (Not a) :+ dml (Not b)
dml (Not a) = Not $ dml a
dml (a :* b) = dml a :* dml b
dml (a :+ b) = dml a :+ dml b

toNNF :: Expr -> Expr
toNNF = rmDoubleNot . dml . toBasis

someFunc = do
    s <- getLine
    let expr = read s :: Expr
    let nnf = toNNF expr
    putStrLn $ "NNF: " ++ (show nnf) 

