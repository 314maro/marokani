module Language.Calc (run) where

import Control.Applicative
import qualified Text.Trifecta as T
import Data.Monoid (mempty)
import Text.PrettyPrint.ANSI.Leijen (plain)

data Expr
  = ExprNum Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr

factor :: T.Parser Expr
factor = (ExprNum . either fromInteger id <$> T.integerOrDouble)
  <|> T.parens expr

factor' :: T.Parser Expr
factor' = factor `T.chainl1` (Pow <$ T.symbolic '^')

term :: T.Parser Expr
term = factor' `T.chainl1` ((Mul <$ T.symbolic '*') <|> (Div <$ T.symbolic '/'))

expr :: T.Parser Expr
expr = term `T.chainl1` ((Add <$ T.symbolic '+') <|> (Sub <$ T.symbolic '-'))

runExpr :: Expr -> Either String Double
runExpr (ExprNum n) = Right n
runExpr (Add m n) = (+) <$> runExpr m <*> runExpr n
runExpr (Sub m n) = (-) <$> runExpr m <*> runExpr n
runExpr (Mul m n) = (*) <$> runExpr m <*> runExpr n
runExpr (Div m n) = do
  m' <- runExpr m
  n' <- runExpr n
  if n' == 0
    then Left "division by 0"
    else return $ m' / n'
runExpr (Pow m n) = do
  m' <- runExpr m
  n' <- runExpr n
  if m' == 0 && n' == 0
    then Left "0 ^ 0"
    else return $ m' ** n'

run :: String -> Either String Double
run s = case T.parseString expr mempty s of
  T.Failure err -> Left $ show $ plain err
  T.Success e -> either (Left . ("runtimeerror: " ++)) Right $ runExpr e
