module Language.MaroKani.Prim (newEnv, newEnv', std) where

import Language.MaroKani.Types

import Paths_marokani

import Data.Traversable (traverse)
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Applicative
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified System.Random as Rand
foreign import ccall unsafe "math.h gamma" gamma :: Double -> Double

mk1Arg :: (String -> Value -> IO Value) -> String -> (String,Value)
mk1Arg f name = (name, PrimFun $ \_ v _ -> f name v)

mk1Arg' :: (String -> ([Expr] -> IO Value) -> Value -> IO Value) -> String -> (String,Value)
mk1Arg' f name = (name, PrimFun $ \ev v _ -> f name ev v)

mk2Args :: (String -> Value -> Value -> IO Value) -> String -> (String,Value)
mk2Args f name = (name, PrimFun $ \_ x _ -> return $ PrimFun $ \_ y _ -> f name x y)

mk2Args' :: (String -> ([Expr] -> IO Value) -> Value -> Value -> IO Value) -> String -> (String,Value)
mk2Args' f name = (name, PrimFun $ \_ x _ -> return $ PrimFun $ \ev y _ -> f name ev x y)

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
primAdd _ (VArray x) (VArray y) = return $ VArray $ x V.++ y
primAdd name (VString _) y = throwM $ TypeMismatch stringName (showType y) name
primAdd name x (VString _) = throwM $ TypeMismatch stringName (showType x) name
primAdd name (VArray _) y = throwM $ TypeMismatch arrayName (showType y) name
primAdd name x (VArray _) = throwM $ TypeMismatch arrayName (showType x) name
primAdd name x y = calcNum (+) (+) VInt VDouble x y name
primSub :: String -> Value -> Value -> IO Value
primSub name x y = calcNum (-) (-) VInt VDouble x y name
primMul :: String -> Value -> Value -> IO Value
primMul _ (VString x) (VInt y) = let y' = fromIntegral y in
  return $ VString $ concat $ replicate y' x
primMul name (VString _) y = throwM $ TypeMismatch intName (showType y) name
primMul _ (VArray x) (VInt y) = let y' = fromIntegral y in
  return $ VArray $ V.concat $ replicate y' x
primMul name (VArray _) y = throwM $ TypeMismatch intName (showType y) name
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

primIndex :: String -> Value -> Value -> IO Value
primIndex _ (VArray arr) (VInt i) = let i' = fromIntegral i in
  maybe (throwM $ IndexOutOfBounds arr i') return $ arr V.!? i'
primIndex _ (VString s) (VInt i) = return $ VString $ take 1 $ drop (fromIntegral i) s
primIndex name (VArray _) i = throwM $ TypeMismatch intName (showType i) name
primIndex name (VString _) i = throwM $ TypeMismatch intName (showType i) name
primIndex name a _ = throwM $ TypeMismatch (stringName `typeOr` arrayName) (showType a) name

primEnumFromTo :: String -> Value -> Value -> IO Value
primEnumFromTo name x y = calcNum V.enumFromTo V.enumFromTo
  (VArray . V.map VInt) (VArray . V.map VDouble) x y name

primHas :: String -> Value -> Value -> IO Value
primHas _ (VObject obj) (VString name) = return $ VBool $ M.member name obj
primHas name (VObject _) y = throwM $ TypeMismatch stringName (showType y) name
primHas name x _ = throwM $ TypeMismatch objectName (showType x) name

apply :: ([Expr] -> IO Value) -> Value -> Value -> IO Value
apply ev f v = ev [App (EValue f) (EValue v)]

primMap :: String -> ([Expr] -> IO Value) -> Value -> Value -> IO Value
primMap _ ev f (VArray arr) = VArray <$> traverse (apply ev f) arr
primMap name _ _ y = throwM $ TypeMismatch arrayName (showType y) name

primAssign :: String -> Value -> Value -> IO Value
primAssign _ (Mutable v) r = liftIO $ atomically $ writeTVar v r >> return r
primAssign name x _ = throwM $ TypeMismatch mutableName (showType x) name

primNewMutable :: String -> Value -> IO Value
primNewMutable _ x = liftIO $ atomically $ Mutable <$> newTVar x

primGetMutable :: String -> Value -> IO Value
primGetMutable _ (Mutable v) = liftIO $ atomically $ readTVar v
primGetMutable name x = throwM $ TypeMismatch mutableName (showType x) name

primAsync :: String -> ([Expr] -> IO Value) -> Value -> IO Value
primAsync _ ev f = VAsync <$> async (apply ev f (VBool False))

primWait :: String -> Value -> IO Value
primWait _ (VAsync a) = wait a
primWait name x = throwM $ TypeMismatch asyncName (showType x) name

primDelay :: String -> Value -> IO Value
primDelay _ (VInt x) = threadDelay (fromIntegral x) >> return (VBool False)
primDelay name x = throwM $ TypeMismatch intName (showType x) name

primPrint :: Value -> Output -> IO Value
primPrint x o = do
  appendOutput o $ show x
  return primPrintV

primPrintV :: Value
primPrintV = PrimFun $ \_ -> primPrint

primTostr :: String -> Value -> IO Value
primTostr _ x = return $ VString $ show x

primShowFun :: String -> Value -> IO Value
primShowFun _ (Fun _ name es) = return $ VString $ "\\" ++ name ++ show es
primShowFun name f = throwM $ TypeMismatch funName (showType f) name

primRandInt :: String -> Value -> IO Value
primRandInt _ _ = VInt <$> Rand.randomIO

primLength :: String -> Value -> IO Value
primLength _ (VArray arr) = return $ VInt $ fromIntegral $ V.length arr
primLength _ (VString s) = return $ VInt $ fromIntegral $ length s
primLength name x = throwM $ TypeMismatch (arrayName `typeOr` stringName) (showType x) name

primSin :: String -> Value -> IO Value
primSin _ (VDouble d) = return $ VDouble $ sin d
primSin _ (VInt i) = return $ VDouble $ sin $ fromIntegral i
primSin name x = throwM $ TypeMismatch doubleName (showType x) name

primCos :: String -> Value -> IO Value
primCos _ (VDouble d) = return $ VDouble $ cos d
primCos _ (VInt i) = return $ VDouble $ cos $ fromIntegral i
primCos name x = throwM $ TypeMismatch doubleName (showType x) name

primTan :: String -> Value -> IO Value
primTan _ (VDouble d) = return $ VDouble $ tan d
primTan _ (VInt i) = return $ VDouble $ tan $ fromIntegral i
primTan name x = throwM $ TypeMismatch doubleName (showType x) name

primGamma :: String -> Value -> IO Value
primGamma _ (VDouble d) = return $ VDouble $ gamma d
primGamma _ (VInt i) = return $ VDouble $ gamma $ fromIntegral i
primGamma name x = throwM $ TypeMismatch doubleName (showType x) name

primFloor :: String -> Value -> IO Value
primFloor _ (VDouble d) = return $ VInt $ floor d
primFloor _ (VInt i) = return $ VInt i
primFloor name x = throwM $ TypeMismatch doubleName (showType x) name

primUnaryPlus :: String -> Value -> IO Value
primUnaryPlus _ (VInt i) = return $ VInt i
primUnaryPlus _ (VDouble d) = return $ VDouble d
primUnaryPlus name x = throwM $ TypeMismatch (intName `typeOr` doubleName) (showType x) name

primUnaryMinus :: String -> Value -> IO Value
primUnaryMinus _ (VInt i) = return $ VInt (- i)
primUnaryMinus _ (VDouble d) = return $ VDouble (- d)
primUnaryMinus name x = throwM $ TypeMismatch (intName `typeOr` doubleName) (showType x) name

primsList :: [(String,Value)]
primsList =
  [ ("true", VBool True)
  , ("false", VBool False)
  , ("pi", VDouble pi)
  , ("print", primPrintV)
  , mk1Arg' primAsync "async"
  , mk1Arg primWait "wait"
  , mk1Arg primDelay "delay"
  , mk1Arg primTostr "tostr"
  , mk1Arg primShowFun "showFun"
  , mk1Arg primRandInt "randInt"
  , mk1Arg primLength "length"
  , mk1Arg primFloor "floor"
  , mk1Arg primSin "sin"
  , mk1Arg primCos "cos"
  , mk1Arg primTan "tan"
  , mk1Arg primGamma "gamma"
  , mk1Arg primUnaryPlus "[+]"
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
  , mk2Args primHas "has"
  , mk2Args' primMap "map"
  , mk1Arg primNewMutable "newMutable"
  , mk1Arg primGetMutable "[*]"
  , mk2Args primAssign "(=)"
  ]

newEnv :: MonadIO m => m Env
newEnv = liftIO $ atomically $ newTVar $ M.fromList primsList

newEnv' :: MonadIO m => EnvC -> m Env
newEnv' e = liftIO $ atomically $ newTVar $ e `M.union` M.fromList primsList

std :: MonadIO m => m String
std = liftIO $ getDataFileName "std.marokani" >>= readFile
