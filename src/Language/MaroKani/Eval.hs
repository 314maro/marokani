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

apply :: (MonadIO m, MonadCatch m) => Env -> Output -> Value -> Value -> m Value
apply _ o (Fun (Just fenv') name body) x = do
  fenv <- liftIO $ atomically $ newTVar $ M.insert name (Left x) fenv'
  eval fenv o body
apply _ _ (Fun Nothing _ _) _ = throwM $ InternalError "環境が Nothing の関数"
apply env o (PrimFun f) x = liftIO $ f (eval env o) x o
apply _ _ f _ = throwM $ TypeMismatch funName (showType f) "apply"

evalF :: (MonadIO m, MonadCatch m) => Env -> Output -> Expr -> m Value
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
evalF env o (EArray exprs) = liftM (VArray . V.fromList) $ mapM (evalF env o) exprs
evalF env o (EObject envList) = do
  let f (name,True,e) = do
        v <- evalF env o e
        return (name, Left v)
      f (name,False,e) = do -- この処理まとめよう
        v <- evalF env o e
        ref <- liftIO $ atomically $ newTVar v
        return (name, Right ref)
  envList' <- mapM f envList
  return $ VObject $ M.fromList envList'
evalF env o (App e1 e2) = do
  v1 <- evalF env o e1
  v2 <- evalF env o e2
  apply env o v1 v2
evalF env o (Multi exprs) = do
  eval env o exprs
evalF env o (ENamespace name decls) = do
  let f (vname,e) = do
        v <- evalF env o e
        return (vname, Left v)
  envList' <- mapM f decls
  let namespace = VObject $ M.fromList envList'
  liftIO $ atomically $ modifyTVar env (M.insert name (Left namespace))
  return namespace
evalF env o (Import exprM name) = do
  let place = "import"
  objEnv <- case exprM of
    Nothing -> liftIO $ atomically $ readTVar env
    Just expr -> do
      v <- evalF env o expr
      case v of
        VObject obj -> return obj
        _ -> throwM $ TypeMismatch namespaceName (showType v) place
  let modify (VObject obj) = liftIO $ atomically $ modifyTVar env (M.union obj)
      modify x = throwM $ TypeMismatch namespaceName (showType x) place
  case M.lookup name objEnv of
    Nothing -> throwM $ UnknownName name place
    Just (Left x) -> modify x
    Just (Right ref) -> liftIO (atomically $ readTVar ref) >>= modify
  return $ VBool False
evalF env o (If cond tru fls) = do
  cond' <- evalF env o cond
  let tru' = evalF env o tru
  let fls' = maybe (return $ VBool False) (evalF env o) fls
  if isTrue cond' then tru' else fls'
evalF env o (While p body) = do
  let loop = do
        b <- evalF env o p
        if isTrue b
          then evalF env o body >> loop
          else return b
  loop
evalF env o (For ini p inc body) = do
  let evalFMaybe = maybe (return ()) (\expr -> evalF env o expr >> return ())
  evalFMaybe ini
  let loop = let loop' = evalF env o body >> evalFMaybe inc >> loop in
        case p of
          Nothing -> loop'
          Just p' -> evalF env o p' >>= \b -> if isTrue b then loop' else return b
  loop
evalF env o (ObjectRef obj name) = do
  let place = "objectRef"
  obj' <- evalF env o obj
  case obj' of
    VObject objEnv -> do
      case M.lookup name objEnv of
        Nothing -> throwM $ UnknownName name place
        Just (Left val) -> return val
        Just (Right ref) -> liftIO $ atomically $ readTVar ref
    _ -> throwM $ TypeMismatch objectName (showType obj') place
evalF env o (Asgn name expr) = do
  let place = "assign"
  val <- evalF env o expr
  result <- liftIO $ atomically $ do
    env' <- readTVar env
    let var = M.lookup name env'
    case var of
      Just (Right ref) -> writeTVar ref val
      _ -> return ()
    return var
  case result of
    Just (Right _) -> return val
    Just (Left _) -> throwM $ TypeMismatch "mutable" "const" place
    Nothing -> throwM $ UnknownName name place
evalF env o (Decl name True expr) = do
  val <- evalF env o expr
  liftIO $ atomically $ modifyTVar env (M.insert name (Left val))
  return val
evalF env o (Decl name False expr) = do
  val <- evalF env o expr
  liftIO $ atomically $ do
    ref <- newTVar val
    modifyTVar env (M.insert name (Right ref))
  return val
evalF env o (ObjectAsgn obj name e) = do
  let place = "objectAsgn"
  obj' <- evalF env o obj
  val <- evalF env o e
  case obj' of
    VObject objEnv -> do
      case M.lookup name objEnv of
        Just (Right ref) -> do
          liftIO $ atomically $ writeTVar ref val
          return obj'
        Just (Left _) -> throwM $ TypeMismatch "mutable" "const" place
        Nothing -> throwM $ UnknownName name place
    _ -> throwM $ TypeMismatch objectName (showType obj') place

eval :: (MonadIO m, MonadCatch m) => Env -> Output -> [Expr] -> m Value
eval env o exprs = foldM (\_ e -> evalF env o e) (VBool False) exprs

eval' :: (MonadIO m, MonadCatch m) => Env -> Output -> [Expr] -> m Value
eval' env o exprs = do
  let Success std' = parse std
  _ <- catchAll (eval env o std') (throwM . InternalError . show)
  eval env o exprs
