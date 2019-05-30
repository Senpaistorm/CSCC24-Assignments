-- How to use: runghc testA4interp.hs

import           System.Environment (getArgs)
import           System.Exit (exitFailure)
import           Test.HUnit
import           Text.Read (readMaybe)

import qualified Data.Map as Map

import           A4 (mainInterp)
import           A4Def

tests =
    [ testEqNum, testLtNum
    , testPlus, testMinus, testMul, testDiv, testMod
    , testCond, testLambda, testLet, testApp, testScope
    , testError
    -- above: 0.25 x 13 = 3.25
    -- below: 0.75
    , testFibFast
    ]

testEqNum = TestList
    [ "7 == 7" ~: mainInterp (Prim2 Eq (Num 7) (Num 7)) ~?= Right (VB True)
    , "7 == 8" ~: mainInterp (Prim2 Eq (Num 7) (Num 8)) ~?= Right (VB False)
    ]

testLtNum = TestList
    [ "7 < 7" ~: mainInterp (Prim2 Lt (Num 7) (Num 7)) ~?= Right (VB False)
    , "7 < 8" ~: mainInterp (Prim2 Lt (Num 7) (Num 8)) ~?= Right (VB True)
    , "8 < 7" ~: mainInterp (Prim2 Lt (Num 8) (Num 7)) ~?= Right (VB False)
    ]

testPlus = "5 + 7" ~: mainInterp (Prim2 Plus (Num 5) (Num 7)) ~?= Right (VN 12)

testMinus = "5 - 7" ~: mainInterp (Prim2 Minus (Num 5) (Num 7)) ~?= Right (VN (-2))

testMul = "5 * 7" ~: mainInterp (Prim2 Mul (Num 5) (Num 7)) ~?= Right (VN 35)

testDiv = TestList
    [ "(-100) div 3" ~:
      mainInterp (Prim2 Div (Num (-100)) (Num 3)) ~?= Right (VN (-34))
    , "1 div 0" ~:
      mainInterp (Prim2 Div (Num 1) (Num 0)) ~?= Left DivByZero
    ]

testMod = TestList
    [ "(-100) mod 3" ~:
      mainInterp (Prim2 Mod (Num (-100)) (Num 3)) ~?= Right (VN 2)
    , "1 mod 0" ~:
      mainInterp (Prim2 Mod (Num 1) (Num 0)) ~?= Left DivByZero
    ]

testCond = TestList
    [ "if True then 1 else 2" ~:
      mainInterp (Cond (Bln True) (Num 1) (Num 2)) ~?= Right (VN 1)
    , "if False then 1 else 2" ~:
      mainInterp (Cond (Bln False) (Num 1) (Num 2)) ~?= Right (VN 2)
    ]

testLambda = "\\v -> v" ~:
    mainInterp (Lambda "v" (Var "v")) ~?= Right (VClosure Map.empty "v" (Var "v"))

testLet = TestList
    [ "let in 0" ~: mainInterp (Let [] (Num 0)) ~?= Right (VN 0)
    , "let x = 7; y = x; in y" ~:
      mainInterp (Let [("x", Num 7), ("y", Var "x")]
                   (Var "y"))
      ~?= Right (VN 7)
    ]

testApp = "(\\v -> v) 43" ~:
    mainInterp (App (Lambda "v" (Var "v")) (Num 43)) ~?= Right (VN 43)

testScope = TestList
    [ "let x=4; in (let x=21; in \\y->x+y) 9"
      ~: mainInterp (Let [("x", Num 4)]
                         (App (Let [("x", Num 21)]
                                   (Lambda "y" (Prim2 Plus (Var "x") (Var "y"))))
                              (Num 9)))
      ~?= Right (VN 30)
    , "let f = \\y -> x+y; x = 0; in f 5"
      ~: mainInterp (Let [ ("f", Lambda "y" (Prim2 Plus (Var "x") (Var "y")))
                         , ("x", Num 0)
                         ]
                      (App (Var "f") (Num 5)))
      ~?= Left VarNotFound
    ]

testError = "let x=2/0; in 1+True" ~:
    mainInterp (Let [("x", Prim2 Div (Num 2) (Num 0))] (Prim2 Plus (Num 1) (Bln True)))
    ~?= Left DivByZero

testFibFast = "fast fib"
    ~: mainInterp this
    ~?= Right (VN 354224848179261915075)
  where
    this = Let [("D", Lambda "x" (App (Var "x") (Var "x"))), ("fib", App (Var "D") (Lambda "self" (Lambda "n" (Cond (Prim2 Lt (Var "n") (Num 2)) (Var "n") (Cond (Prim2 Eq (Var "n") (Num 2)) (Num 1) (Let [("k",Prim2 Div (Var "n") (Num 2)),("r",Prim2 Mod (Var "n") (Num 2)),("fk",App (App (Var "D") (Var "self")) (Var "k")),("fk1",App (App (Var "D") (Var "self")) (Prim2 Plus (Var "k") (Num 1)))] (Cond (Prim2 Eq (Var "r") (Num 1)) (Prim2 Plus (Prim2 Mul (Var "fk") (Var "fk")) (Prim2 Mul (Var "fk1") (Var "fk1"))) (Prim2 Minus (Prim2 Mul (Prim2 Mul (Num 2) (Var "fk")) (Var "fk1")) (Prim2 Mul (Var "fk") (Var "fk"))))))))))] (App (Var "fib") (Num 100))
-- let
--   D = \x -> x x;
--   fib = D (
--     \self -> \n ->
--       if n < 2 then n
--       else if n == 2 then 1
--       else let k = n / 2;
--                r = n % 2;
--                fk = D self k;
--                fk1 = D self (k+1);
--            in if r == 1
--               then fk*fk + fk1*fk1
--               else 2*fk*fk1 - fk*fk
--   );
-- in fib 100

main = do
    args <- getArgs
    case args of
      a:_ | Just n <- readMaybe a, 0 <= n, n < length tests ->
            do c@Counts{errors=e, failures=f} <- runTestTT (tests !! n)
               if e == 0 && f == 0
                   then return c
                   else exitFailure
          | otherwise -> error "No such test number."
      _ -> runTestTT (TestList tests)
