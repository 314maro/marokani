{-# LANGUAGE TupleSections #-}

module Language.MaroKani.Eval
  ( eval
  , eval'
  ) where

import Language.MaroKani.Types
import Language.MaroKani.Prim
import Language.MaroKani.Parser

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Vector as V

apply :: (MonadIO m, MonadCatch m) => Env -> Output -> Value -> Value -> m Value
apply _ o (VFun fenvc name body) x = do
  fenv <- liftIO $ atomically $ newTVar $ M.insert name x fenvc
  eval fenv o body
apply env o (PrimFun f) x = do
  envc <- liftIO $ atomically $ readTVar env
  liftIO $ f envc (eval env o) x o
apply _ _ f _ = throwM $ TypeMismatch funName (showType f) "apply"

ifExpr :: (MonadIO m, MonadCatch m) => Env -> Output -> Expr -> m a -> m a -> m a
ifExpr env o p tru fls = do
  b <- evalF env o p
  if isTrue b then tru else fls

whenExpr :: (MonadIO m, MonadCatch m) => Env -> Output -> Expr -> m Value -> m Value
whenExpr env o p tru = do
  b <- evalF env o p
  if isTrue b then tru else return b

lookupVar :: (MonadIO m, MonadCatch m) => Env -> String -> String -> m Value
lookupVar env name place = do
  envc <- liftIO $ atomically $ readTVar env
  lookupVar' envc name place

lookupVar' :: (MonadCatch m) => EnvC -> String -> String -> m Value
lookupVar' envc name place = do
  case M.lookup name envc of
    Nothing -> throwM $ UnknownName name place
    Just val -> return $ val

evalF :: (MonadIO m, MonadCatch m) => Env -> Output -> Expr -> m Value
evalF env _ (Var name) = lookupVar env name "ref"
evalF _ _ (EValue val) = return val
evalF env _ (EFun name0 names exprs) = do
  envc <- liftIO $ atomically $ readTVar env
  let body [] = exprs
      body (name1:names') = [EFun name1 names' exprs]
  return $ VFun envc name0 $ body names
evalF env o (EArray exprs) = (VArray . V.fromList) `liftM` mapM (evalF env o) exprs
evalF env o (EObject envList) = (VObject . M.fromList) `liftM`
  mapM (\(name,e) -> (name,) `liftM` evalF env o e) envList
evalF env o (App e1 e2) = do
  v1 <- evalF env o e1
  v2 <- evalF env o e2
  apply env o v1 v2
evalF env o (Multi exprs) = do
  eval env o exprs
evalF env o (ENamespace name decls) = do
  let f (vname,e) = do
        v <- evalF env o e
        return (vname, v)
  envList' <- mapM f decls
  let namespace = VObject $ M.fromList envList'
  liftIO $ atomically $ modifyTVar env (M.insert name namespace)
  return namespace
evalF env o (Import exprM name) = do
  let place = "import"
  objEnvc <- case exprM of
    Nothing -> liftIO $ atomically $ readTVar env
    Just expr -> do
      v <- evalF env o expr
      case v of
        VObject obj -> return obj
        _ -> throwM $ TypeMismatch namespaceName (showType v) place
  v <- lookupVar' objEnvc name place
  case v of
    VObject obj -> liftIO $ atomically $ modifyTVar env (M.union obj)
    x -> throwM $ TypeMismatch namespaceName (showType x) place
  return $ VBool False
evalF env o (If p tru fls) = do
  let tru' = evalF env o tru
  let fls' = maybe (return $ VBool False) (evalF env o) fls
  ifExpr env o p tru' fls'
evalF env o (While p body) = do
  let loop = whenExpr env o p (evalF env o body >> loop)
  loop
evalF env o (For ini p inc body) = do
  let evalFMaybe = maybe (return ()) (\expr -> evalF env o expr >> return ())
  evalFMaybe ini
  let loop1 = evalF env o body >> evalFMaybe inc >> loop
      loop = case p of
        Nothing -> loop1
        Just p' -> whenExpr env o p' loop1
  loop
evalF env o (ObjectRef objE name) = do
  let place = "objectRef"
  objV <- evalF env o objE
  case objV of
    VObject objEnvc -> lookupVar' objEnvc name place
    _ -> throwM $ TypeMismatch objectName (showType objV) place
evalF env o (Decl name expr) = do
  val <- evalF env o expr
  liftIO $ atomically $ modifyTVar env (M.insert name val)
  return val

eval :: (MonadIO m, MonadCatch m) => Env -> Output -> [Expr] -> m Value
eval env o exprs = foldM (\_ e -> evalF env o e) (VBool False) exprs

eval' :: (MonadIO m, MonadCatch m) => Env -> Output -> [Expr] -> m Value
eval' env o exprs = do
  _ <- catchAll (std >>= parseIO >>= eval env o) (throwM . InternalError)
  eval env o exprs
