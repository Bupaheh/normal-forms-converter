module Main where

import Lib

main :: IO ()
main = do
    s <- getLine
    let expr = readExpr s
    let nnf = toNNF expr
    let dnf = toDNF expr
    let cnf = toCNF expr
    putStrLn $ "NNF: " ++ (show nnf) 
    putStrLn $ "DNF: " ++ (show dnf) 
    putStrLn $ "CNF: " ++ (show cnf) 
