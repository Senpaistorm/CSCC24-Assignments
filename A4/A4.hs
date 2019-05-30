module A4 where

import           Control.Applicative
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           A4Def
import           ParserLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Expr)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser mainParser inp
    return ans

mainParser :: Parser Expr
mainParser = whitespaces *> block <* eof
    where 
        block = cond <|> lambda <|> letp <|> myInfix

        cond = do
            keyword "if"
            ifblock <- block
            keyword "then"
            thenblock <- block
            keyword "else"
            elseblock <- block
            pure (Cond ifblock thenblock elseblock)
        
        lambda = do
            operator "\\"
            variable <- var
            operator "->"
            ldblock <- block
            pure (Lambda variable ldblock)

        letp = do
            keyword "let"
            eqns <- many equation
            keyword "in"
            inblock <- block
            pure (Let eqns inblock)

        equation = do
            v <- var
            terminal "="
            e <- block
            terminal ";"
            pure (v, e)

        myInfix = do
            a <- arith
            (do c <- cmp
                b <- arith
                return (Prim2 c a b))
             <|> return a

        arith = chainl1 addend addop
        addend = chainl1 factor mulop

        factor = fmap (foldl1 App) (some atom)
        atom =  between (char '(' *> whitespaces)
                        (char ')' *> whitespaces)
                        block
                        <|> literal <|> (fmap Var var)
                                
        var = identifier ["let", "in", "True", "False", "if", "then", "else"]

        literal = fmap Num integer <|> fmap Bln boolean

        boolean = (terminal "True" *> return True)
                    <|>
                  (terminal "False" *> return False)

        cmp = (operator "==" *> pure Eq)
              <|> 
              (operator "<" *> pure Lt)

        addop = fmap Prim2 (
            operator "+" *> return Plus 
            <|> 
            operator "-" *> return Minus)

        mulop = fmap Prim2 (
            operator "*" *> return Mul 
            <|> 
            operator "/" *> return Div 
            <|> 
            operator "%" *> return Mod)


mainInterp :: Expr -> Either Error Value
mainInterp = interp Map.empty

interp :: Map String Value -> Expr -> Either Error Value
interp env (Var v) = case Map.lookup v env of
  Nothing -> Left VarNotFound
  Just val -> Right val

interp env (Num i) = Right (VN i)

interp env (Bln b) = Right (VB b)

interp env (Prim2 Eq e1 e2) =
    case (interp env e1, interp env e2) of
      (Right (VN i), Right (VN j)) -> Right (VB (i == j))
      (Right (VB b), Right (VB c)) -> Right (VB (b == c))
      _ -> Left TypeError

interp env (Prim2 Lt e1 e2) =
    case (interp env e1, interp env e2) of
      (Right (VN i), Right (VN j)) -> Right (VB (i < j))
      _ -> Left TypeError

interp env (Prim2 Plus e1 e2) =
    case (interp env e1, interp env e2) of
      (Right (VN i), Right (VN j)) -> Right (VN (i + j))
      _ -> Left TypeError

interp env (Prim2 Minus e1 e2) =
    case (interp env e1, interp env e2) of
      (Right (VN i), Right (VN j)) -> Right (VN (i - j))
      _ -> Left TypeError

interp env (Prim2 Mul e1 e2) =
    case (interp env e1, interp env e2) of
      (Right (VN i), Right (VN j)) -> Right (VN (i * j))
      _ -> Left TypeError

interp env (Prim2 Div e1 e2) =
    case (interp env e1, interp env e2) of
      (_, Right (VN 0)) -> Left DivByZero
      (Right (VN i), Right (VN j)) -> Right (VN (div i j))
      _ -> Left TypeError

interp env (Prim2 Mod e1 e2) =
    case (interp env e1, interp env e2) of
      (_, Right (VN 0)) -> Left DivByZero
      (Right (VN i), Right (VN j)) -> Right (VN (mod i j))
      _ -> Left TypeError

interp env (Cond t e1 e2) = case interp env t of
  Right (VB True) -> interp env e1
  Right (VB False) -> interp env e2
  _ -> Left TypeError

interp env (Lambda var body) = Right (VClosure env var body)

interp env (Let [] body) = interp env body

interp env (Let ((v,rhs) : defs) body) = do
    a <- interp env rhs
    let env' = Map.insert v a env
    interp env' (Let defs body)

interp env (App f e) = do
    c <- interp env f
    case c of
      VClosure fEnv v body -> do
          eVal <- interp env e
          let bEnv = Map.insert v eVal fEnv  -- fEnv, not env
          interp bEnv body
      _ -> Left TypeError
