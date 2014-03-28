module Network.MaroKani.Internal
( newId
, enter
, exit
, say
, soon
, comet
, delete
, deleteAll
, updateReq
) where

import Network.MaroKani.Types

import Control.Applicative
import Network.HTTP.Conduit
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy.Char8 as B (unpack)

kaniURL :: String
kaniURL = kaniURL' ++ "CometServlet"

kaniURL' :: String
kaniURL' = "http://comet.kanichat.com/rent/"

kani :: KaniMode -> KaniConfig -> KaniRequest -> IO KaniResponse
kani mode config req = do
  let req' = (kaniFun config req) { reqMode = Just mode }
  httpReq <- parseUrl $ kaniQuery kaniURL req'
  let httpReq' = httpReq
        { responseTimeout = Just $ 60*1000*1000, proxy = kaniProxy config }
  res <- withManager $ httpLbs httpReq'
  let str = responseBody res
  maybe (error $ "JSON: " ++ B.unpack str) return $ decode str

newId :: KaniConfig -> KaniRequest -> IO KaniResponse
newId = kani NewIdMode

enter :: KaniConfig -> KaniRequest -> IO KaniResponse
enter = kani EnterMode

exit :: KaniConfig -> KaniRequest -> IO KaniResponse
exit = kani ExitMode

say :: String -> KaniConfig -> KaniRequest -> IO KaniResponse
say msg config = kani WriteMode
  config { kaniFun = \req -> req { reqMessage = Just msg } }

soon :: KaniConfig -> KaniRequest -> IO KaniResponse
soon = kani LogSoonMode

comet :: KaniConfig -> KaniRequest -> IO KaniResponse
comet = kani LogCometMode

delete :: Integer -> KaniConfig -> KaniRequest -> IO KaniResponse
delete logno config = kani Delete1Mode
  config { kaniFun = \req -> req { reqLogno = Just logno } }

deleteAll :: KaniConfig -> KaniRequest -> IO KaniResponse
deleteAll = kani DeleteAllMode

updateReq :: KaniResponse -> KaniRequest -> KaniRequest
updateReq res req = req
  { reqId = Just $ resSessionId res
  , reqLogno = newLogno <|> reqLogno req
  }
  where
    safeLast [] = Nothing
    safeLast [x] = Just x
    safeLast (_:xs) = safeLast xs
    newLogno = resLog res >>= safeLast >>= \x -> return $ logLogno x + 1
