module Network.MaroKani
( KaniRequest(..)
, defaultRequest
, KaniResponse(..)
, KaniLog(..)
, Memdata(..)
, KaniConfig(..)
, defaultConfig
, Proxy(..)
, Kani
, runKani
, runKani'
, kaniIO
, asyncKani
, updateName
, updateIcon
, newId
, newId_
, enter
, enter_
, exit
, exit_
, say
, say_
, soon
, soon_
, comet
, comet_
, delete
, delete_
, deleteAll
, deleteAll_
, update
, update_
) where

import Network.MaroKani.Types
import qualified Network.MaroKani.Internal as K
import Control.Monad.Reader
import Control.Monad.Catch
import Control.Concurrent.STM
import Control.Concurrent.Async

type Kani = ReaderT (KaniConfig, TVar KaniRequest) IO

connection :: (KaniConfig -> KaniRequest -> IO KaniResponse) -> Kani KaniResponse
connection con = ReaderT $ \(config,reqv) -> do
  req <- atomically $ readTVar reqv
  res <- catchAll (con config req) (\e -> print e >> return undefined)
  atomically $ modifyTVar reqv (K.updateReq res)
  return res

asyncKani :: Kani a -> Kani (Async a)
asyncKani m = do
  x <- ask
  liftIO $ async $ runReaderT m x

updateName :: String -> Kani ()
updateName name = do
  (_,reqv) <- ask
  liftIO $ atomically $ modifyTVar reqv (\req -> req { reqName = name })

updateIcon :: String -> Kani ()
updateIcon name = do
  (_,reqv) <- ask
  liftIO $ atomically $ modifyTVar reqv (\req -> req { reqKanappsIcon = Just name })

newId :: Kani KaniResponse
newId = connection K.newId
newId_ :: Kani ()
newId_ = void newId

enter :: Kani KaniResponse
enter = connection K.enter
enter_ :: Kani ()
enter_ = void enter

exit :: Kani KaniResponse
exit = connection K.exit
exit_ :: Kani ()
exit_ = void exit

say :: String -> Kani KaniResponse
say = connection . K.say
say_ :: String -> Kani ()
say_ = void . say

soon :: Kani KaniResponse
soon = connection K.soon
soon_ :: Kani ()
soon_ = void soon

comet :: Kani KaniResponse
comet = connection K.comet
comet_ :: Kani ()
comet_ = void comet

delete :: Integer -> Kani KaniResponse
delete = connection . K.delete
delete_ :: Integer -> Kani ()
delete_ = void . delete

deleteAll :: Kani KaniResponse
deleteAll = connection K.deleteAll
deleteAll_ :: Kani ()
deleteAll_ = void deleteAll

update :: Kani KaniResponse
update = connection K.update
update_ :: Kani ()
update_ = void update

runKani :: KaniConfig -> KaniRequest -> Kani a -> IO a
runKani config request m = do
  reqv <- atomically $ newTVar request
  runReaderT m (config,reqv)

runKani' :: KaniRequest -> Kani a -> IO a
runKani' req m = runKani defaultConfig req m

kaniIO :: Kani a -> Kani (IO a)
kaniIO act = do
  env <- ask
  return $ runReaderT act env
