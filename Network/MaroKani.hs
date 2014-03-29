{-# LANGUAGE DeriveFunctor #-}

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
, asyncKani
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
) where

import Network.MaroKani.Types
import qualified Network.MaroKani.Internal as K
import Control.Monad (void)
import Control.Monad.Trans (MonadIO, liftIO)
import Control.Monad.Free
import Control.Applicative
import Control.Concurrent.STM
import Control.Concurrent.Async

-- Freeモナド使わなくても出来るかも
data KaniF a = KaniF (KaniConfig -> TVar KaniRequest -> IO a)
  deriving (Functor)

type Kani = Free KaniF

instance MonadIO Kani where
  liftIO a = wrap $ KaniF $ \_ _ -> return <$> a

connection :: (KaniConfig -> KaniRequest -> IO KaniResponse) -> Kani KaniResponse
connection con = wrap $ KaniF $ \config reqv -> do
  req <- atomically $ readTVar reqv
  res <- con config req
  atomically $ modifyTVar reqv (K.updateReq res)
  return $ return res

asyncKani :: Kani a -> Kani (Async a)
asyncKani m = wrap $ KaniF $ \config reqv -> do
  req <- atomically $ readTVar reqv
  return <$> async (runKani config req m)

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

runKani :: KaniConfig -> KaniRequest -> Kani a -> IO a
runKani config request m = do
  reqv <- atomically $ newTVar request
  let go (Pure a) = return a
      go (Free (KaniF act)) = act config reqv >>= go
  go m

runKani' :: KaniRequest -> Kani a -> IO a
runKani' req m = runKani defaultConfig req m
