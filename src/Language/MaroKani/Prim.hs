module Language.MaroKani.Prim (newEnv, std) where

import Language.MaroKani.Types
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Applicative
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified System.Random as Rand

mk2Args :: (Value -> Value -> IO Value) -> Value
mk2Args f = PrimFun $ \x _ -> return $ PrimFun $ \y _ -> f x y

calcNum :: (Integer -> Integer -> ti) -> (Double -> Double -> td)
  -> (ti -> a) -> (td -> a) -> Value -> Value -> String -> IO a
calcNum i _ c _ (VInt x) (VInt y) _ = return $ c $ x `i` y
calcNum _ d _ c (VInt x) (VDouble y) _ = return $ c $ fromIntegral x `d` y
calcNum _ d _ c (VDouble x) (VInt y) _ = return $ c $ x `d` fromIntegral y
calcNum _ d _ c (VDouble x) (VDouble y) _ = return $ c $ x `d` y
calcNum _ _ _ _ (VInt _) y s = throwM $ TypeMismatch (intName `typeOr` doubleName) (showType y) s
calcNum _ _ _ _ (VDouble _) y s = throwM $ TypeMismatch (intName `typeOr` doubleName) (showType y) s
calcNum _ _ _ _ x _ s = throwM $ TypeMismatch (intName `typeOr` doubleName) (showType x) s

primAdd :: Value -> Value -> IO Value
primAdd (VString x) (VString y) = return $ VString $ x ++ y
primAdd (VString _) y = throwM $ TypeMismatch stringName (showType y) "+"
primAdd x (VString _) = throwM $ TypeMismatch stringName (showType x) "+"
primAdd x y = calcNum (+) (+) VInt VDouble x y "+"
primSub :: Value -> Value -> IO Value
primSub x y = calcNum (-) (-) VInt VDouble x y "-"
primMul :: Value -> Value -> IO Value
primMul x y = calcNum (*) (*) VInt VDouble x y "*"
primDiv :: Value -> Value -> IO Value
primDiv x y = calcNum div (/) VInt VDouble x y "/"
primMod :: Value -> Value -> IO Value
primMod (VInt x) (VInt y) = return $ VInt $ x `mod` y
primMod (VInt _) y = throwM $ TypeMismatch intName (showType y) "%"
primMod x _ = throwM $ TypeMismatch intName (showType x) "%"
primPow :: Value -> Value -> IO Value
primPow x y = calcNum (^) (**) VInt VDouble x y "^"
primLT :: Value -> Value -> IO Value
primLT x y = calcNum (<) (<) VBool VBool x y "<"
primLE :: Value -> Value -> IO Value
primLE x y = calcNum (<=) (<=) VBool VBool x y "<="
primGT :: Value -> Value -> IO Value
primGT x y = calcNum (>) (>) VBool VBool x y ">"
primGE :: Value -> Value -> IO Value
primGE x y = calcNum (>=) (>=) VBool VBool x y ">="
primNE :: Value -> Value -> IO Value
primNE x y = return $ VBool $ x /= y
primEQ :: Value -> Value -> IO Value
primEQ x y = return $ VBool $ x == y

primEnumFromTo :: Value -> Value -> IO Value
primEnumFromTo x y = calcNum V.enumFromTo V.enumFromTo
  (VArray . V.map VInt) (VArray . V.map VDouble) x y "--->"

primIndex :: Value -> Value -> IO Value
primIndex (VArray arr) (VInt i) = return $ arr V.! fromIntegral i
primIndex (VString s) (VInt i) = return $ VString $ take 1 $ drop (fromIntegral i) s
primIndex (VArray _) i = throwM $ TypeMismatch intName (showType i) "!"
primIndex (VString _) i = throwM $ TypeMismatch intName (showType i) "!"
primIndex a _ = throwM $ TypeMismatch (stringName `typeOr` arrayName) (showType a) "!"

primTostr :: Value -> TChan String -> IO Value
primTostr x _ = return $ VString $ show x

primPrint :: Value -> TChan String -> IO Value
primPrint x chan = do
  atomically $ writeTChan chan (show x)
  return x    

primRandInt :: Value -> TChan String -> IO Value
primRandInt _ _ = VInt <$> Rand.randomIO

primShowFun :: Value -> TChan String -> IO Value
primShowFun (Fun _ name es) _ = return $ VString $ "\\" ++ name ++ show es
primShowFun f _ = throwM $ TypeMismatch funName (showType f) "showFun"

primsList :: [([Char], Value)]
primsList =
  [ ("true", VBool True)
  , ("false", VBool False)
  , ("tostr", PrimFun primTostr)
  , ("showFun", PrimFun primShowFun)
  , ("print", PrimFun primPrint)
  , ("randInt", PrimFun primRandInt)
  , ("+", mk2Args primAdd)
  , ("-", mk2Args primSub)
  , ("*", mk2Args primMul)
  , ("/", mk2Args primDiv)
  , ("%", mk2Args primMod)
  , ("^", mk2Args primPow)
  , ("<", mk2Args primLT)
  , ("<=", mk2Args primLE)
  , (">", mk2Args primGT)
  , (">=", mk2Args primGE)
  , ("!=", mk2Args primNE)
  , ("==", mk2Args primEQ)
  , ("!", mk2Args primIndex)
  , ("--->", mk2Args primEnumFromTo)
  ]

newEnv :: MonadIO m => m Env
newEnv = liftIO $ do
  atomically $ newTVar $ M.map Left $ M.fromList primsList

std :: String
std = "If = \\b x y {if b then x else y};"
   ++ "(.) = \\f g x {f (g x)};"
   ++ "($) = \\f x {f x};"
