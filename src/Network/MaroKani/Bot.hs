module Network.MaroKani.Bot
  ( mkReacts
  , Script
  , everyone
  , colonsEnd
  , colonsSep
  , nameIs
  , isSelf
  , isOwner
  , Condition
  ) where

import Network.MaroKani

import Control.Monad
import Control.Monad.Reader
import Data.List (stripPrefix)

type Condition = ReaderT (KaniResponse,KaniLog) Maybe Bool

type Script = ReaderT (KaniResponse,KaniLog) Maybe (Kani ())

everyone :: Condition
everyone = return True

nameIs :: String -> Condition
nameIs s = do
  (_,kaniLog) <- ask
  return $ s == logName kaniLog

isSelf :: Condition
isSelf = do
  (res,_) <- ask
  nameIs $ maybe "" id $ memName $ resMydata res

isOwner :: Condition
isOwner = do
  (res,kaniLog) <- ask
  let myIp = memIp $ resMydata res
  let ip = logIp kaniLog
  b <- isSelf
  return $ ip == myIp && not b

stripPrefixes :: [String] -> String -> Maybe String
stripPrefixes prefixes msg = foldM (\s pre -> stripPrefix pre s >>= stripColon) msg prefixes
  where
    stripColon (':':str) = Just str
    stripColon ('：':str) = Just str
    stripColon _ = Nothing

authorize :: Condition -> Script -> Script
authorize auth script = do
  b <- auth
  if b then script else return $ say_ "権限がない"

colonsEnd :: Condition -> Condition -> [String] -> (String -> Kani ()) -> Script
colonsEnd cond auth prefixes act = do
  cond >>= guard
  (_,kaniLog) <- ask
  s <- lift $ prefixes `stripPrefixes` logMessage kaniLog
  authorize auth $ return $ act s

colonsSep :: Condition -> Condition -> [String] -> String -> Kani () -> Script
colonsSep cond auth prefixes cmdName act = do
  cond >>= guard
  (_,kaniLog) <- ask
  s <- lift $ prefixes `stripPrefixes` logMessage kaniLog
  guard (s == cmdName)
  authorize auth $ return act

runScript :: KaniResponse -> KaniLog -> Script -> Maybe (Kani ())
runScript res kaniLog script = runReaderT script (res,kaniLog)

mkReact :: [Script] -> KaniResponse -> KaniLog -> Kani ()
mkReact xs res kaniLog = maybe (return ()) id
  $ msum $ map (runScript res kaniLog) xs

mkReacts :: [Script] -> KaniResponse -> Kani ()
mkReacts acts res = case resLog res of
  Nothing -> return ()
  Just x -> mapM_ (asyncKani . mkReact acts res) x
