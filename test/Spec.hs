import Lib
import Expr
import Helper
import Data.List
import Test.QuickCheck

varSize :: Int
varSize = 10
              
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
prop_read_show expr = isEq (read (show expr)) expr

main :: IO ()
main = quickCheck prop_read_show
