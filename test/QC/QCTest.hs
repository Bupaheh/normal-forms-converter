{-# LANGUAGE TemplateHaskell #-}

import Lib
import Expr
import Shared
import Data.List
import Test.QuickCheck

varSize :: Int
varSize = 6
              
instance Arbitrary Expr where
  arbitrary = sized arbitrary' 
    where var = Var <$> ("a" ++) <$> show <$> choose (1, varSize) 
          arbitrary' 0 = var
          arbitrary' n = oneof 
            [ var
            , (:&) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
            , (:|) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
            , (:=>) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
            , (:<=>) <$> resize (n `div` 2) arbitrary <*> resize (n `div` 2) arbitrary
            , Not <$> resize (n - 1) arbitrary
            ]
        
prop_read_show :: Expr -> Bool
prop_read_show expr = show (read e :: Expr) == e
  where e = show expr

prop_test_NNF :: Expr -> Bool
prop_test_NNF expr = isNNF nnf && isEq nnf expr
  where nnf = toNNF expr

prop_test_DNF :: Expr -> Bool
prop_test_DNF expr = isDNF dnf && isEq dnf expr
  where dnf = toDNF expr

prop_test_CNF :: Expr -> Bool
prop_test_CNF expr = isCNF cnf && isEq cnf expr
  where cnf = toCNF expr
  
return []
main :: IO Bool
main = $forAllProperties $
  quickCheckWithResult (stdArgs {maxSuccess = 100, maxSize = 17})
