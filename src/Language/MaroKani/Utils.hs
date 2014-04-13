module Language.MaroKani.Utils
  ( value2string
  , value2integer
  , defaultvalue
  , mkIOValue
  , mkIOValue'
  , mkIOValueVoid
  , mk1Arg
  , mk1Arg'
  , mk1ArgVoid
  , mk2Args
  , mk2Args'
  , fromList
  , Value(..)
  , EnvC
  ) where

import Language.MaroKani.Types

import Control.Monad.Catch
import qualified Data.Map as M (fromList)

fromList :: [(String, Value)] -> EnvC
fromList = M.fromList

defaultvalue :: Value
defaultvalue = VBool False

value2string :: String -> Value -> IO String
value2string _ (VString s) = return s
value2string name v = throwM $ TypeMismatch stringName (showType v) name

value2integer :: String -> Value -> IO Integer
value2integer _ (VInt i) = return i
value2integer name v = throwM $ TypeMismatch intName (showType v) name

mkIOValue :: (String -> IO Value) -> String -> (String,Value)
mkIOValue f name = (name, PrimFun $ \_ _ _ _ -> f name)

mkIOValue' :: (String -> ([Expr] -> IO Value) -> IO Value) -> String -> (String,Value)
mkIOValue' f name = (name, PrimFun $ \_ ev _ _ -> f name ev)

mkIOValueVoid :: (String -> IO a) -> String -> (String,Value)
mkIOValueVoid f name = (name, PrimFun $ \_ _ _ _ -> f name >> return defaultvalue)

mk1Arg :: (String -> Value -> IO Value) -> String -> (String,Value)
mk1Arg f name = (name, PrimFun $ \_ _ v _ -> f name v)

mk1Arg' :: (String -> ([Expr] -> IO Value) -> Value -> IO Value) -> String -> (String,Value)
mk1Arg' f name = (name, PrimFun $ \_ ev v _ -> f name ev v)

mk1ArgVoid :: (String -> Value -> IO a) -> String -> (String,Value)
mk1ArgVoid f name = (name, PrimFun $ \_ _ v _ -> f name v >> return defaultvalue)

mk2Args :: (String -> Value -> Value -> IO Value) -> String -> (String,Value)
mk2Args f name = (name, PrimFun $ \_ _ x _ -> return $ PrimFun $ \_ _ y _ -> f name x y)

mk2Args' :: (String -> ([Expr] -> IO Value) -> Value -> Value -> IO Value) -> String -> (String,Value)
mk2Args' f name = (name, PrimFun $ \_ _ x _ -> return $ PrimFun $ \_ ev y _ -> f name ev x y)
