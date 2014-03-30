{-# LANGUAGE DeriveDataTypeable #-}

module Language.MaroKani.Types
( Env
, Env'
, Value(..)
, isTrue
, Expr(..)
, MaroKaniException(..)
) where

import Control.Monad.Catch
import Control.Concurrent.STM
import Data.Typeable (Typeable)
import qualified Data.Map as M
import qualified Data.Vector as V
import Text.PrettyPrint.ANSI.Leijen (Doc, plain)

type Env' = M.Map String (Either Value (TVar Value))
type Env = TVar Env'

data Value
  = VInt Integer
  | VDouble Double
  | VString String
  | VBool Bool
  | VArray (V.Vector Value)
  | Fun (Maybe Env') String [Expr]
  | PrimFun (Value -> TChan String -> IO Value)
instance Show Value where
  show (VInt i) = show i
  show (VDouble d) = show d
  show (VString s) = s
  show (VBool True) = "true"
  show (VBool False) = "false"
  show (VArray arr) = '[' : V.ifoldr f "]" arr
    where
      f 0 v s = shows v s
      f _ v s = ',' : shows v s
  show (Fun _ _ _) = "<<fun>>"
  show (PrimFun _) = "<<prim-fun>>"
instance Eq Value where
  VInt x == VInt y = x == y
  VDouble x == VDouble y = x == y
  VString x == VString y = x == y
  VBool x == VBool y = x == y
  VArray x == VArray y = x == y
  _ == _ = False
isTrue :: Value -> Bool
isTrue (VBool False) = False
isTrue _ = True

data Expr
  = Var String
  | EValue Value
  | EArray [Expr]
  | App Expr Expr
  | If Expr Expr (Maybe Expr)
  | Asgn String Expr
  | Decl String Expr
  deriving (Show)

data MaroKaniException
  = TypeMismatch String String
  | UnknownName String
  | ParserError Doc
  | Default String
  deriving (Typeable)
instance Show MaroKaniException where
  show (TypeMismatch t1 t2) = "型エラー: " ++ t1 ++ " のはずが " ++ t2
  show (UnknownName name) = "知らない名前: " ++ name
  show (ParserError doc) = show $ plain doc
  show (Default s) = "エラー: " ++ s
instance Exception MaroKaniException where
