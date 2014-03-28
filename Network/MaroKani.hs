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
, enter
, exit
, say
, soon
, comet
, delete
, deleteAll
) where

import Network.MaroKani.Types
import qualified Network.MaroKani.Internal as K
import Control.Monad
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

enter :: Kani KaniResponse
enter = connection K.enter

exit :: Kani KaniResponse
exit = connection K.exit

say :: String -> Kani KaniResponse
say = connection . K.say

soon :: Kani KaniResponse
soon = connection K.soon

comet :: Kani KaniResponse
comet = connection K.comet

delete :: Integer -> Kani KaniResponse
delete = connection . K.delete

deleteAll :: Kani KaniResponse
deleteAll = connection K.deleteAll

runKani :: KaniConfig -> KaniRequest -> Kani a -> IO a
runKani config request m = do
  reqv <- atomically $ newTVar request
  let go (Pure a) = return a
      go (Free (KaniF act)) = act config reqv >>= go
  go m

runKani' :: KaniRequest -> Kani a -> IO a
runKani' req m = runKani defaultConfig req m
