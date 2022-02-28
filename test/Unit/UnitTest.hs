import Expr
import Shared
import Test.Tasty
import Test.Tasty.HUnit

main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [readShowTests] 

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
