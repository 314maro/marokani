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
  -> (ti -> a) -> (td -> a) -> Value -> Value -> Maybe a
calcNum i _ c _ (VInt x) (VInt y) = Just $ c $ x `i` y
calcNum _ d _ c (VInt x) (VDouble y) = Just $ c $ fromIntegral x `d` y
calcNum _ d _ c (VDouble x) (VInt y) = Just $ c $ x `d` fromIntegral y
calcNum _ d _ c (VDouble x) (VDouble y) = Just $ c $ x `d` y
calcNum _ _ _ _ _ _ = Nothing

primAdd :: Value -> Value -> IO Value
primAdd x y = maybe (throwM $ TypeMismatch "+" "+") return
  $ calcNum (+) (+) VInt VDouble x y <|> strApp x y
  where
    strApp (VString x') (VString y') = Just $ VString $ x' ++ y'
    strApp _ _ = Nothing
primSub :: Value -> Value -> IO Value
primSub x y = maybe (throwM $ TypeMismatch "-" "-") return $ calcNum (-) (-) VInt VDouble x y
primMul :: Value -> Value -> IO Value
primMul x y = maybe (throwM $ TypeMismatch "*" "*") return $ calcNum (*) (*) VInt VDouble x y
primDiv :: Value -> Value -> IO Value
primDiv x y = maybe (throwM $ TypeMismatch "/" "/") return $ calcNum div (/) VInt VDouble x y
primMod :: Value -> Value -> IO Value
primMod (VInt x) (VInt y) = return $ VInt $ x `mod` y
primMod _ _ = throwM $ TypeMismatch "%" "%"
primPow :: Value -> Value -> IO Value
primPow x y = maybe (throwM $ TypeMismatch "^" "^") return $ calcNum (^) (**) VInt VDouble x y
primLT :: Value -> Value -> IO Value
primLT x y = maybe (throwM $ TypeMismatch "<" "<") return $ calcNum (<) (<) VBool VBool x y
primLE :: Value -> Value -> IO Value
primLE x y = maybe (throwM $ TypeMismatch "<=" "<=") return $ calcNum (<=) (<=) VBool VBool x y
primGT :: Value -> Value -> IO Value
primGT x y = maybe (throwM $ TypeMismatch ">" ">") return $ calcNum (>) (>) VBool VBool x y
primGE :: Value -> Value -> IO Value
primGE x y = maybe (throwM $ TypeMismatch ">=" ">=") return $ calcNum (>=) (>=) VBool VBool x y
primNE :: Value -> Value -> IO Value
primNE x y = return $ VBool $ x /= y
primEQ :: Value -> Value -> IO Value
primEQ x y = return $ VBool $ x == y

primIndex :: Value -> Value -> IO Value
primIndex (VArray arr) (VInt i) = return $ arr V.! fromIntegral i
primIndex (VString s) (VInt i) = return $ VString $ take 1 $ drop (fromIntegral i) s
primIndex (VArray _) _ = throwM $ TypeMismatch "Int" "??"
primIndex (VString _) _ = throwM $ TypeMismatch "Int" "??"
primIndex _ (VInt _) = throwM $ TypeMismatch "Array" "??"
primIndex _ _ = throwM $ TypeMismatch "!" "!"

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
primShowFun _ _ = throwM $ TypeMismatch "fun" "??"

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
  ]

newEnv :: MonadIO m => m Env
newEnv = liftIO $ do
  atomically $ newTVar $ M.map Left $ M.fromList primsList

std :: String
std = "If = \\b x y {if b then x else y};"
   ++ "(.) = \\f g x {f (g x)};"
   ++ "($) = \\f x {f x};"
