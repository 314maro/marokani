module Language.MaroKani.Eval (eval, eval') where

import Language.MaroKani.Types
import Language.MaroKani.Prim
import Language.MaroKani.Parser
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Applicative
import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Vector as V

apply :: (MonadIO m, MonadCatch m) => TChan String -> Value -> Value -> m Value
apply chan (Fun (Just fenv') name body) x = do
  fenv <- liftIO $ atomically $ newTVar $ M.insert name (Left x) fenv'
  eval fenv chan body
apply _ (Fun Nothing _ _) _ = throwM $ InternalError "環境が Nothing の関数"
apply chan (PrimFun f) x = liftIO $ f x chan
apply _ f _ = throwM $ TypeMismatch funName (showType f) "apply"

evalF :: (MonadIO m, MonadCatch m) => Env -> TChan String -> Expr -> m Value
evalF env _ (Var name) = do
  result <- liftIO $ atomically $ do
    env' <- readTVar env
    case M.lookup name env' of
      Nothing -> return Nothing
      Just (Left val) -> return $ Just val
      Just (Right ref) -> Just <$> readTVar ref
  case result of
    Nothing -> throwM $ UnknownName name "ref"
    Just val -> return val
evalF env _ (EValue (Fun Nothing name expr)) = do
  env' <- liftIO $ atomically $ readTVar env
  return $ Fun (Just env') name expr
evalF _ _ (EValue val) = return val
evalF env chan (EArray exprs) = liftM (VArray . V.fromList) $ mapM (evalF env chan) exprs
evalF env chan (EObject envList) = do
  let f (name,True,e) = do
        v <- evalF env chan e
        return (name, Left v)
      f (name,False,e) = do -- この処理まとめよう
        v <- evalF env chan e
        ref <- liftIO $ atomically $ newTVar v
        return (name, Right ref)
  envList' <- mapM f envList
  return $ VObject $ M.fromList envList'
evalF env chan (App e1 e2) = do
  v1 <- evalF env chan e1
  v2 <- evalF env chan e2
  apply chan v1 v2
evalF env chan (If cond tru fls) = do
  cond' <- evalF env chan cond
  let tru' = evalF env chan tru
  let fls' = maybe (return $ VBool False) (evalF env chan) fls
  if isTrue cond' then tru' else fls'
evalF env chan (ObjectRef obj name) = do
  obj' <- evalF env chan obj
  case obj' of
    VObject objEnv -> do
      case M.lookup name objEnv of
        Nothing -> throwM $ UnknownName name "objectRef"
        Just (Left val) -> return val
        Just (Right ref) -> liftIO $ atomically $ readTVar ref
    _ -> throwM $ TypeMismatch objectName (showType obj') "objectRef"
evalF env chan (Asgn name expr) = do
  val <- evalF env chan expr
  result <- liftIO $ atomically $ do
    env' <- readTVar env
    let var = M.lookup name env'
    case var of
      Just (Right ref) -> writeTVar ref val
      _ -> return ()
    return var
  case result of
    Just (Right _) -> return val
    Just (Left _) -> throwM $ TypeMismatch "mutable" "const" "assign"
    Nothing -> throwM $ UnknownName name "assign"
evalF env chan (Decl name True expr) = do
  val <- evalF env chan expr
  liftIO $ atomically $ modifyTVar env (M.insert name (Left val))
  return val
evalF env chan (Decl name False expr) = do
  val <- evalF env chan expr
  liftIO $ atomically $ do
    ref <- newTVar val
    modifyTVar env (M.insert name (Right ref))
  return val
evalF env chan (ObjectAsgn obj name e) = do
  obj' <- evalF env chan obj
  val <- evalF env chan e
  case obj' of
    VObject objEnv -> do
      case M.lookup name objEnv of
        Just (Right ref) -> do
          liftIO $ atomically $ writeTVar ref val
          return obj'
        Just (Left _) -> throwM $ TypeMismatch "mutable" "const" "objectAsgn"
        Nothing -> throwM $ UnknownName name "objectAsgn"
    _ -> throwM $ TypeMismatch objectName (showType obj') "objectAsgn"

eval :: (MonadIO m, MonadCatch m) => Env -> TChan String -> [Expr] -> m Value
eval env chan exprs = foldM (\_ e -> evalF env chan e) (VBool False) exprs

eval' :: (MonadIO m, MonadCatch m) => Env -> TChan String -> [Expr] -> m Value
eval' env chan exprs = do
  let Success std' = parse std
  _ <- catchAll (eval env chan std') (throwM . InternalError . show)
  eval env chan exprs
