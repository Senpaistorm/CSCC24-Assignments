-- How to use: runghc testA4parser.hs

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Test.HUnit
import Text.Read (readMaybe)

import A4Def
import A4 (mainParser)
import ParserLib

parse = runParser mainParser

tests =
    [ testHandout, testBig
    -- above: 0.4 x  2 = 0.8 marks
    -- below: 0.2 x 16 = 3.2 marks
    , testJunk, testSpaces
    , testNum, testBool, testVar, testApp
    , testMul, testAdd, testCmp
    , testInfix,  testParen0
    , testCond, testLambda, testLet0, testLet
    , testPLI
    ]

testHandout = "handout"
    ~: parse inp
    ~?= (Just (Let [("x", Prim2 Mul (Num 4) (Prim2 Plus (Num 5) (Num 6)))]
                   (Lambda "y" (Cond (Prim2 Lt (Var "x") (Var "y"))
                                     (Var "x")
                                     (Var "y")))))
  where
    inp = "  let x \n     = 4 * ( \n  5+6) ; \n in \\ y ->  if  x< y then x else y\n"

testBig = "big test"
    ~: parse inp
    ~?= answer
  where
    inp = "\\v -> \\w -> (let f = \\ x -> 32 - x ; n = 42 + 3 ; in f (n * n) ) + (if 1 - 1 < w then v / w  else (\\x -> x) (w / v))"
    answer = Just (Lambda "v" (Lambda "w" (Prim2 Plus (Let [("f",Lambda "x" (Prim2 Minus (Num 32) (Var "x"))),("n",Prim2 Plus (Num 42) (Num 3))] (App (Var "f") (Prim2 Mul (Var "n") (Var "n")))) (Cond (Prim2 Lt (Prim2 Minus (Num 1) (Num 1)) (Var "w")) (Prim2 Div (Var "v") (Var "w")) (App (Lambda "x" (Var "x")) (Prim2 Div (Var "w") (Var "v")))))))

testJunk = "junk after 0" ~: parse "0 (" ~?= Nothing

testSpaces = "leading and trailing spaces"
    ~: parse "   0    "
    ~?= Just (Num 0)

testNum = "one number" ~: parse "45" ~?= Just (Num 45)

testBool = "one bool" ~: [ "True" ~: parse "True" ~?= Just (Bln True)
                         , "False" ~: parse "False" ~?= Just (Bln False)
                         ]

testVar = "one var" ~: parse "CscC24" ~?= Just (Var "CscC24")

testApp = "f 5 x"
    ~: parse "f 5 x"
    ~?= Just (App (App (Var "f") (Num 5)) (Var "x"))

testMul = "5 % 6 / 7 * 8"
    ~: parse "5 % 6 / 7 * 8"
    ~?= Just (Prim2 Mul (Prim2 Div (Prim2 Mod (Num 5) (Num 6)) (Num 7)) (Num 8))

testAdd = "5 - 6 + 7 - 8"
    ~: parse "5 - 6 + 7 - 8"
    ~?= Just (Prim2 Minus (Prim2 Plus (Prim2 Minus (Num 5) (Num 6)) (Num 7)) (Num 8))

testCmp = "cmp"
    ~: [ "5 == 6" ~: parse "5 == 6" ~?= Just (Prim2 Eq (Num 5) (Num 6))
       , "5 < 6" ~: parse "5 < 6" ~?= Just (Prim2 Lt (Num 5) (Num 6))
       ]

testInfix = "medium size infix expr"
    ~: parse "31 * 32 + 33 / 34 == sin 35 - atan2 36"
    ~?= Just (Prim2 Eq
               (Prim2 Plus (Prim2 Mul (Num 31) (Num 32))
                           (Prim2 Div (Num 33) (Num 34)))
               (Prim2 Minus (App (Var "sin") (Num 35))
                            (App (Var "atan2") (Num 36))))

testParen0 = "(0)" ~: parse "(0)" ~?= Just (Num 0)

testCond = "if-then-else"
    ~: parse "if True then 1 else 2"
    ~?= Just (Cond (Bln True) (Num 1) (Num 2))

testLambda = "lambda"
    ~: parse "\\ v -> v"
    ~?= Just (Lambda "v" (Var "v"))

testLet0 = "let in 0" ~: parse "let in 0" ~?= Just (Let [] (Num 0))

testLet = "non-empty let"
    ~: parse "let x = 0 ; y = 0 ; in 0"
    ~?= Just (Let [("x", Num 0), ("y", Num 0)] (Num 0))

-- PL/I had no reserved words. Look it up! Interesting history. Taught in North
-- American CS in the 1970s, including UofT. (To be sure, only a minority of
-- schools had CS at all back then.)
testPLI = "PL/I"
    ~: parse "else + then * in"
    ~?= Nothing

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
