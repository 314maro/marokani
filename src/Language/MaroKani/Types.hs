{-# LANGUAGE DeriveDataTypeable #-}

module Language.MaroKani.Types
( Env
, Env'
, Output
, appendOutput
, Value(..)
, isTrue
, showIO
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

import Control.Monad.Trans
import Control.Monad.Catch
import Control.Concurrent.STM
import Data.Typeable (Typeable)
import Control.Applicative
import Data.Traversable (traverse)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Vector as V
import Text.PrettyPrint.ANSI.Leijen (Doc, plain)

type Env' = M.Map String (Either Value (TVar Value))
type Env = TVar Env'

type Output = TVar (String -> String)

appendOutput :: (MonadIO m) => Output -> String -> m ()
appendOutput o s = liftIO $ atomically $ modifyTVar o (. (s ++))

data Value
  = VInt Integer
  | VDouble Double
  | VString String
  | VBool Bool
  | VArray (V.Vector Value)
  | VObject Env'
  | Fun (Maybe Env') String [Expr]
  | PrimFun (([Expr] -> IO Value) -> Value -> Output -> IO Value)
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
  show (VObject env) = "{" ++ (intercalate "," $ M.keys env) ++ "}"
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
showIO :: Value -> IO String
showIO (VObject env) = do
  let f (Left l) = return $ "::=" ++ show l
      f (Right r) = (\x -> ":=" ++ show x) <$> atomically (readTVar r)
  list <- M.assocs <$> traverse f env
  let s = intercalate "," $ map (\(name,val) -> name ++ val) list
  return $ "{" ++ s ++ "}"
showIO x = return $ show x
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
  | EObject [(String,Bool,Expr)]
  | App Expr Expr
  | Multi [Expr]
  | If Expr Expr (Maybe Expr)
  | ObjectRef Expr String
  | Asgn String Expr
  | Decl String Bool Expr
  | ObjectAsgn Expr String Expr
  deriving (Show)

data MaroKaniException
  = TypeMismatch String String String
  | UnknownName String String
  | ParserError Doc
  | InternalError String
  | StringTooLong
  | TimeOver
  | Default String
  deriving (Typeable)
showPlace :: String -> String
showPlace s = " 場所: " ++ s
instance Show MaroKaniException where
  show (TypeMismatch t1 t2 p) = "型エラー: " ++ t1 ++ " のはずが " ++ t2 ++ showPlace p
  show (UnknownName name p) = "知らない名前: " ++ name ++ showPlace p
  show (ParserError doc) = show $ plain doc
  show (InternalError s) = "内部のエラー: " ++ s
  show StringTooLong = "出力長すぎ"
  show TimeOver = "時間かかりすぎ"
  show (Default s) = "エラー: " ++ s
instance Exception MaroKaniException where
