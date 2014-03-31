{-# LANGUAGE DeriveDataTypeable #-}

module Language.MaroKani.Types
( Env
, Env'
, Value(..)
, isTrue
, showType
, intName
, doubleName
, stringName
, boolName
, arrayName
, objectName
, funName
, primFunName
, typeOr
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
  | VObject Env
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
  show (VObject _) = "<<object>>"
  show (Fun _ _ _) = "<<fun>>"
  show (PrimFun _) = "<<prim-fun>>"
instance Eq Value where
  VInt x == VInt y = x == y
  VDouble x == VDouble y = x == y
  VString x == VString y = x == y
  VBool x == VBool y = x == y
  VArray x == VArray y = x == y
  _ == _ = False
  Fun _ _ _ /= _ = False
  _ /= Fun _ _ _ = False
  PrimFun _ /= _ = False
  _ /= PrimFun _ = False
  VObject _ /= _ = False
  _ /= VObject _ = False
  x /= y = not $ x == y
isTrue :: Value -> Bool
isTrue (VBool False) = False
isTrue _ = True
showType :: Value -> String
showType (VInt _) = intName
showType (VDouble _) = doubleName
showType (VString _) = stringName
showType (VBool _) = boolName
showType (VArray _) = arrayName
showType (VObject _) = objectName
showType (Fun _ _ _) = funName
showType (PrimFun _) = primFunName
intName :: String
intName = "int"
doubleName :: String
doubleName = "double"
stringName :: String
stringName = "string"
boolName :: String
boolName = "bool"
arrayName :: String
arrayName = "array"
objectName :: String
objectName = "object"
funName :: String
funName = "fun"
primFunName :: String
primFunName = "prim-fun"
typeOr :: String -> String -> String
typeOr x y = x ++ " か " ++ y

data Expr
  = Var String
  | EValue Value
  | EArray [Expr]
  | EObject [(String,Expr)]
  | App Expr Expr
  | If Expr Expr (Maybe Expr)
  | Asgn String Expr
  | Decl String Expr
  | ConstDecl String Expr
  deriving (Show)

data MaroKaniException
  = TypeMismatch String String String
  | UnknownName String String
  | ParserError Doc
  | InternalError String
  | Default String
  deriving (Typeable)
showPlace :: String -> String
showPlace s = " 場所: " ++ s
instance Show MaroKaniException where
  show (TypeMismatch t1 t2 p) = "型エラー: " ++ t1 ++ " のはずが " ++ t2 ++ showPlace p
  show (UnknownName name p) = "知らない名前: " ++ name ++ showPlace p
  show (ParserError doc) = show $ plain doc
  show (InternalError s) = "内部のエラー: " ++ s
  show (Default s) = "エラー: " ++ s
instance Exception MaroKaniException where
