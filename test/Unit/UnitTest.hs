import Expr
import Lib
import Shared
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [readShowTests, toNNFTests, toDNFTests, toCNFTests] 

showRead :: String -> String
showRead s = show (read s :: Expr)

readShowTests = testGroup "Read and show tests"
  [ testCase "Basic read and show test" $ 
      "a <=> ~(a & b => c | d)" @=? showRead "a <=> ~ (a & b => c | d)"
  , testCase "Double Not test" $
      "a | ~~a" @=? showRead "a | ~~a"
  , testCase "Without spaces test" $
      "a <=> ~(a & b => c | d)" @=? showRead "a<=>~(a&b=>c|d)"
  , testCase "Redundant brackets test" $
      "a <=> ~(a & b => c | d)" @=? showRead "a <=> (~((a & b) => (c | d)))"
  , testCase "Implication left associativity test" $
      "a => b => c" @=? showRead "(a => b) => c"
  , testCase "Implication right associativity test" $
      "a => (b => c)" @=? showRead "a => (b => c)"
  ]
  
var = read "a" :: Expr
expr = read "a <=> ~(a & b => c | d)" :: Expr
bigExpr = read "(~(a2 => a2 => a4) | (a6 | a5 => (a6 <=> a6)) => (~~((a4 <=> a3) | a4) => ~(a3 => (a5 => a3)))) | ((a1 <=> a5) & a1 & a1 <=> ~a2 & a3 & ~~a4 & ((a5 => a5) | a2 & a1))" :: Expr

testNNF :: Expr -> Bool
testNNF expr = isNNF nnf && isEq nnf expr
  where nnf = toNNF expr

toNNFTests = testGroup "toNNF tests"
  [ testCase "Variable test" $ 
      assertBool "" $ testNNF var
  , testCase "Basic expression test" $
      assertBool "" $ testNNF expr
  , testCase "Big expression test" $
      assertBool "" $ testNNF bigExpr
  ]
  
testDNF :: Expr -> Bool
testDNF expr = isDNF dnf && isEq dnf expr
  where dnf = toDNF expr

toDNFTests = testGroup "toDNF tests"
  [ testCase "Variable test" $ 
      assertBool "" $ testDNF var
  , testCase "Basic expression test" $
      assertBool "" $ testDNF expr
  , testCase "Big expression test" $
      assertBool "" $ testDNF bigExpr
  ]
  
testCNF :: Expr -> Bool
testCNF expr = isCNF cnf && isEq cnf expr
  where cnf = toCNF expr

toCNFTests = testGroup "toCNF tests"
  [ testCase "Variable test" $ 
      assertBool "" $ testCNF var
  , testCase "Basic expression test" $
      assertBool "" $ testCNF expr
  , testCase "Big expression test" $
      assertBool "" $ testCNF bigExpr
  ]
