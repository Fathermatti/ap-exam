-- This is a suggested skeleton for your main black-box tests. You are not
-- required to use Tasty, but be sure that your test suite can be build
-- and run against any implementation of the APQL APIs.
  
import Types
import Parser
import Preprocessor
import Engine
-- Do not import from the XXXImpl modules here!

import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Set as S
import qualified Data.Map as M

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) tests

tests :: TestTree
tests = testGroup
  "Tests"
  [ atomTests ]

atomTests = testGroup
  "Atom tests"
  [ let t = "x()" in 
    testCase t $ parseString t @?= Right [Rule (Atom "x" []) CTrue ],
   let t = " x() " in testCase t $ parseString t @?= Right [Rule (Atom "x" []) CTrue ],
   let t =  "x(t)" in  testCase t $ parseString t @?=  Right [Rule (Atom "x" [TVar "t"]) CTrue], 
   let t =  "x(\"y\") "  in  testCase t $ parseString t @?=  Right [Rule (Atom "x" [TData "y"]) CTrue],
   let t =  "x(\"y\", z) "  in  testCase t $ parseString t @?=  Right [Rule (Atom "x" [TData "y",TVar "z"]) CTrue]
   
   ]

-- testCaseBad s t =
--   testCase ("*" ++ s) $
--     case t of
--       Right a -> assertFailure $ "Unexpected success: " ++ show a
--       Left (EUser _) -> return () -- any message is fine
--       Left em -> assertFailure $ "Error: " ++ show em

-- rudimentary :: TestTree
-- rudimentary =
--  testGroup "Rudimentary tests"
--    [testCase "parse1" $
--       parseString pgmStr @?= Right pgmAST,
--     testCaseBad "parse2" $
--       parseString "p(x) if .",
--     testCase "clausify1" $
--       clausify pgmAST @?= Right pgmIDB,
--     testCaseBad "clausify2" $
--       clausify [Rule (Atom "p" [TVar "x"]) CTrue],
--     testCase "stratify1" $ -- too strict! other correct answers also possible
--       stratify pgmIDB [("r",1)] @?= Right pgmStratX,
--     testCaseBad "stratify2" $
--       stratify (IDB [("p",0)]
--                     [Clause (Atom "p" []) [] [TNot (Atom "p" [])]]) [],
--     testCase "execute" $
--       fmap M.fromList (execute pgmIDB pgmStratX [(("r",1), pgmExtR)])
--         @?= Right (M.fromList pgmEDB) ]
--  where
--    pgmStr = "p(x,y) if q(x) and r(y). q(\"a\")."
--    pgmAST = [Rule (Atom "p" [TVar "x", TVar "y"])
--                   (CAnd (CAtom (Atom "q" [TVar "x"]))
--                         (CAtom (Atom "r" [TVar "y"]))),
--              Rule (Atom "q" [TData "a"])
--                   CTrue]
--    pgmIDB = IDB [("p", 2), ("q",1)]
--                 [Clause (Atom "p" [TVar "x", TVar "y"])
--                         [Atom "q" [TVar "x"], Atom "r" [TVar "y"]]
--                         [],
--                  Clause (Atom "q" [TData "a"]) [] []]
--    pgmStratX = [[("p",2), ("q",1)]]
--    pgmExtR = S.fromList [["b"], ["c"]]
--    pgmExtQ = S.fromList [["a"]]
--    pgmExtP = S.fromList [["a", "b"], ["a", "c"]]
--    pgmEDB = [(("p",2),pgmExtP), (("q",1), pgmExtQ), (("r",1), pgmExtR)]
