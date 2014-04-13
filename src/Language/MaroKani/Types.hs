{-# LANGUAGE DeriveDataTypeable #-}

module Language.MaroKani.Types
  ( Env
  , EnvC
  , Output
  , appendOutput
  , Value(..)
  , isTrue
  , showType
  , intName
  , doubleName
  , stringName
  , boolName
  , arrayName
  , objectName
  , namespaceName
  , funName
  , primFunName
  , asyncName
  , mutableName
  , typeOr
  , Expr(..)
  , MaroKaniException(..)
  , showColor
  ) where

import Control.Monad.Trans
import Control.Monad.Catch
import Control.Concurrent.Async (Async)
import Control.Concurrent.STM
import Data.Typeable (Typeable)
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Vector as V
import Text.PrettyPrint.ANSI.Leijen (Doc, plain)

type EnvC = M.Map String Value
type Env = TVar EnvC

type Output = TVar (String -> String)

appendOutput :: (MonadIO m) => Output -> String -> m ()
appendOutput o s = liftIO $ atomically $ modifyTVar o (. (s ++))

data Value
  = VInt Integer
  | VDouble Double
  | VString String
  | VBool Bool
  | VArray (V.Vector Value)
  | VObject EnvC
  | VFun EnvC String [Expr]
  | PrimFun (EnvC -> ([Expr] -> IO Value) -> Value -> Output -> IO Value)
  | VAsync (Async Value)
  | Mutable (TVar Value)
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
  show (VObject env) = "{"
    ++ (intercalate "," $ map (\(k,v) -> k ++ "::=" ++ show v) $ M.assocs env)
    ++ "}"
  show (VFun _ _ _) = "<<fun>>"
  show (PrimFun _) = "<<prim-fun>>"
  show (VAsync _) = "<<async>>"
  show (Mutable _) = "<<mutable>>"
instance Eq Value where
  VInt x == VInt y = x == y
  VDouble x == VDouble y = x == y
  VString x == VString y = x == y
  VBool x == VBool y = x == y
  VArray x == VArray y = x == y
  _ == _ = False
  VFun _ _ _ /= _ = False
  _ /= VFun _ _ _ = False
  PrimFun _ /= _ = False
  _ /= PrimFun _ = False
  VObject _ /= _ = False
  _ /= VObject _ = False
  VAsync _ /= _ = False
  _ /= VAsync _ = False
  Mutable _ /= _ = False
  _ /= Mutable _ = False
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
showType (VFun _ _ _) = funName
showType (PrimFun _) = primFunName
showType (VAsync _) = asyncName
showType (Mutable _) = mutableName
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
namespaceName :: String
namespaceName = "namespace"
funName :: String
funName = "fun"
primFunName :: String
primFunName = "prim-fun"
asyncName :: String
asyncName = "async"
mutableName :: String
mutableName = "mutable"
typeOr :: String -> String -> String
typeOr x y = x ++ " か " ++ y

data Expr
  = Var String
  | EValue Value
  | EFun String [String] [Expr]
  | EArray [Expr]
  | EObject [(String,Expr)]
  | App Expr Expr
  | Multi [Expr]
  | ENamespace String [(String,Expr)]
  | Import (Maybe Expr) String
  | If Expr Expr (Maybe Expr)
  | While Expr Expr
  | For (Maybe Expr) (Maybe Expr) (Maybe Expr) Expr
  | ObjectRef Expr String
  | Decl String Expr
  deriving (Show)

data MaroKaniException
  = TypeMismatch String String String
  | UnknownName String String
  | ParserError Doc
  | InternalError SomeException
  | IndexOutOfBounds (V.Vector Value) Int
  | StringTooLong
  | TimeOver Int
  | Default String
  deriving (Typeable)
showPlace :: String -> String
showPlace s = " 場所: " ++ s
showInternal :: String -> String
showInternal = ("内部のエラー: " ++)
instance Show MaroKaniException where
  show (TypeMismatch t1 t2 p) = "型エラー: " ++ t1 ++ " のはずが " ++ t2 ++ showPlace p
  show (UnknownName name p) = "知らない名前: " ++ name ++ showPlace p
  show (ParserError doc) = show $ plain doc
  show (InternalError e) = showInternal $ show e
  show (IndexOutOfBounds arr i) = "インデックスでかすぎ: " ++ show (VArray arr) ++ ", " ++ show i
  show StringTooLong = "出力長すぎ"
  show (TimeOver t) = "時間かかりすぎ: " ++ shows t "s"
  show (Default s) = "エラー: " ++ s
instance Exception MaroKaniException where
showColor :: MaroKaniException -> String
showColor (ParserError doc) = show doc
showColor (InternalError e) = showInternal $ maybe (show e) showColor $ fromException e
showColor e = show e
