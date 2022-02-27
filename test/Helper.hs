module Helper(isEq, isNNF, isDNF) where

import Data.List
import Test.QuickCheck
import Expr

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x : xs) = x : (unique $ filter (/= x) xs)

allInt :: Int -> [[Bool]]
allInt 0 = [[]]
allInt n = concat $ map (\l -> [True : l, False : l]) list
  where list = allInt (n - 1)

getVars :: Expr -> [String]
getVars = sort . unique . getVars'

getVars' :: Expr -> [String]
getVars' (Var a) = [a]
getVars' (Not a) = getVars' a
getVars' (a :& b) = getVars' a ++ getVars' b
getVars' (a :| b) = getVars' a ++ getVars' b
getVars' (a :=> b) = getVars' a ++ getVars' b
getVars' (a :<=> b) = getVars' a ++ getVars' b

getRes :: [(String, Bool)] -> Expr -> Bool
getRes int (Var a) = case lookup a int of 
                          Just b -> b
                          Nothing -> undefined
getRes int (Not a) = not $ getRes int a
getRes int (a :& b) = getRes int a && getRes int b
getRes int (a :| b) = getRes int a || getRes int b
getRes int (a :<=> b) = getRes int a == getRes int b
getRes int (a :=> b) = not a' || b'
  where a' = getRes int a
        b' = getRes int b
        
isEq' :: [String] -> [Bool] -> Expr -> Expr -> Bool
isEq' vars int a b = getRes int' a == getRes int' b
  where int' = zip vars int

isEq :: Expr -> Expr -> Bool
isEq a b = (vars1 == vars2) && all (\l -> isEq' vars1 l a b) int
  where vars1 = getVars a
        vars2 = getVars b
        int = allInt $ length vars1
        
        
isNNF :: Expr -> Bool
isNNF (a :& b) = isNNF a && isNNF b
isNNF (a :| b) = isNNF a && isNNF b
isNNF (Var a) = True
isNNF (Not (Var a)) = True
isNNF a = False

isDNF :: Expr -> Bool
isDNF (a :& (b :| c)) = False
isDNF ((a :| b) :& c) = False
isDNF (a :& b) = isDNF a && isDNF b
isDNF (a :| b) = isDNF a && isDNF b
isDNF (Var a) = True
isDNF (Not (Var a)) = True
isDNF a = False

