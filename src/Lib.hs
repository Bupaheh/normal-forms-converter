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
toBasis (a :<-> b) = (Not aa :+ bb) :* (Not bb :+ aa)
    where aa = toBasis a
          bb = toBasis b

someFunc = putStrLn $ show t1

