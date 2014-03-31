module Language.MaroKani.Prim (newEnv, std) where

import Language.MaroKani.Types
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Applicative
import Data.Traversable (traverse)
import Data.List (intercalate)
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified System.Random as Rand

mk1Arg :: (String -> Value -> IO Value) -> String -> (String,Value)
mk1Arg f name = (name, PrimFun $ \v _ -> f name v)

mk2Args :: (String -> Value -> Value -> IO Value) -> String -> (String,Value)
mk2Args f name = (name, PrimFun $ \x _ -> return $ PrimFun $ \y _ -> f name x y)

calcNum :: (Integer -> Integer -> ti) -> (Double -> Double -> td)
  -> (ti -> a) -> (td -> a) -> Value -> Value -> String -> IO a
calcNum i _ c _ (VInt x) (VInt y) _ = return $ c $ x `i` y
calcNum _ d _ c (VInt x) (VDouble y) _ = return $ c $ fromIntegral x `d` y
calcNum _ d _ c (VDouble x) (VInt y) _ = return $ c $ x `d` fromIntegral y
calcNum _ d _ c (VDouble x) (VDouble y) _ = return $ c $ x `d` y
calcNum _ _ _ _ (VInt _) y s = throwM $ TypeMismatch (intName `typeOr` doubleName) (showType y) s
calcNum _ _ _ _ (VDouble _) y s = throwM $ TypeMismatch (intName `typeOr` doubleName) (showType y) s
calcNum _ _ _ _ x _ s = throwM $ TypeMismatch (intName `typeOr` doubleName) (showType x) s

primAdd :: String -> Value -> Value -> IO Value
primAdd _ (VString x) (VString y) = return $ VString $ x ++ y
primAdd name (VString _) y = throwM $ TypeMismatch stringName (showType y) name
primAdd name x (VString _) = throwM $ TypeMismatch stringName (showType x) name
primAdd name x y = calcNum (+) (+) VInt VDouble x y name
primSub :: String -> Value -> Value -> IO Value
primSub name x y = calcNum (-) (-) VInt VDouble x y name
primMul :: String -> Value -> Value -> IO Value
primMul name x y = calcNum (*) (*) VInt VDouble x y name
primDiv :: String -> Value -> Value -> IO Value
primDiv name x y = calcNum div (/) VInt VDouble x y name
primMod :: String -> Value -> Value -> IO Value
primMod _ (VInt x) (VInt y) = return $ VInt $ x `mod` y
primMod name (VInt _) y = throwM $ TypeMismatch intName (showType y) name
primMod name x _ = throwM $ TypeMismatch intName (showType x) name
primPow :: String -> Value -> Value -> IO Value
primPow name x y = calcNum (^) (**) VInt VDouble x y name
primLT :: String -> Value -> Value -> IO Value
primLT name x y = calcNum (<) (<) VBool VBool x y name
primLE :: String -> Value -> Value -> IO Value
primLE name x y = calcNum (<=) (<=) VBool VBool x y name
primGT :: String -> Value -> Value -> IO Value
primGT name x y = calcNum (>) (>) VBool VBool x y name
primGE :: String -> Value -> Value -> IO Value
primGE name x y = calcNum (>=) (>=) VBool VBool x y name
primNE :: String -> Value -> Value -> IO Value
primNE _ x y = return $ VBool $ x /= y
primEQ :: String -> Value -> Value -> IO Value
primEQ _ x y = return $ VBool $ x == y

primEnumFromTo :: String -> Value -> Value -> IO Value
primEnumFromTo name x y = calcNum V.enumFromTo V.enumFromTo
  (VArray . V.map VInt) (VArray . V.map VDouble) x y name

primIndex :: String -> Value -> Value -> IO Value
primIndex _ (VArray arr) (VInt i) = return $ arr V.! fromIntegral i
primIndex _ (VString s) (VInt i) = return $ VString $ take 1 $ drop (fromIntegral i) s
primIndex name (VArray _) i = throwM $ TypeMismatch intName (showType i) name
primIndex name (VString _) i = throwM $ TypeMismatch intName (showType i) name
primIndex name a _ = throwM $ TypeMismatch (stringName `typeOr` arrayName) (showType a) name

primPrint :: Value -> TChan String -> IO Value
primPrint x chan = do
  atomically $ writeTChan chan (show x)
  return x    

primTostr :: String -> Value -> IO Value
primTostr _ x = return $ VString $ show x

primShowFun :: String -> Value -> IO Value
primShowFun _ (Fun _ name es) = return $ VString $ "\\" ++ name ++ show es
primShowFun name f = throwM $ TypeMismatch funName (showType f) name

primShowObj :: String -> Value -> IO Value
primShowObj _ (VObject env) = do
  env' <- atomically $ readTVar env
  list <- M.assocs <$> traverse (either return (atomically . readTVar)) env'
  let s = intercalate "," $ map (\(name,val) -> name ++ ":=" ++ show val) list
  return $ VString s
primShowObj name x = throwM $ TypeMismatch objectName (showType x) name

primRandInt :: String -> Value -> IO Value
primRandInt _ _ = VInt <$> Rand.randomIO

primFloor :: String -> Value -> IO Value
primFloor _ (VDouble d) = return $ VInt $ floor d
primFloor _ (VInt i) = return $ VInt i
primFloor name x = throwM $ TypeMismatch doubleName (showType x) name

primUnaryMinus :: String -> Value -> IO Value
primUnaryMinus _ (VInt i) = return $ VInt (- i)
primUnaryMinus _ (VDouble d) = return $ VDouble (- d)
primUnaryMinus name x = throwM $ TypeMismatch (intName `typeOr` doubleName) (showType x) name

primsList :: [(String,Value)]
primsList =
  [ ("true", VBool True)
  , ("false", VBool False)
  , ("print", PrimFun primPrint)
  , mk1Arg primTostr "tostr"
  , mk1Arg primShowFun "showFun"
  , mk1Arg primShowObj "showObj"
  , mk1Arg primRandInt "randInt"
  , mk1Arg primFloor "floor"
  , mk1Arg primUnaryMinus "[-]"
  , mk2Args primAdd "(+)"
  , mk2Args primSub "(-)"
  , mk2Args primMul "(*)"
  , mk2Args primDiv "(/)"
  , mk2Args primMod "(%)"
  , mk2Args primPow "(^)"
  , mk2Args primLT "(<)"
  , mk2Args primLE "(<=)"
  , mk2Args primGT "(>)"
  , mk2Args primGE "(>=)"
  , mk2Args primNE "(!=)"
  , mk2Args primEQ "(==)"
  , mk2Args primIndex "(!)"
  , mk2Args primEnumFromTo "(--->)"
  ]

newEnv :: MonadIO m => m Env
newEnv = liftIO $ do
  atomically $ newTVar $ M.map Left $ M.fromList primsList

std :: String
std = "If ::= \\b x y {if b then x else y};"
   ++ "(<<) ::= \\f g x {f (g x)};"
   ++ "($) ::= \\f x {f x};"
