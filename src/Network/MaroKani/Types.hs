module Network.MaroKani.Types
( KaniMode(..)
, KaniRequest(..)
, defaultRequest
, kaniQuery
, kaniQuery'
, KaniResponse(..)
, KaniLog(..)
, Memdata(..)
, KaniConfig(..)
, defaultConfig
, Proxy(..)
) where

import Control.Applicative
import Network.HTTP (urlEncode)
import Network.HTTP.Conduit
import Data.Aeson
import Data.Word (Word8)
import Numeric (showHex)
import Data.Monoid (mempty)

data TextSize = Small | Normal | Large
instance Show TextSize where
  show Small = "small"
  show Normal = "normal"
  show Large = "large"

data KaniMode = NewIdMode | GetDataMode | UpdateMode
  | LogSoonMode | LogCometMode
  | EnterMode | ExitMode
  | WriteMode | Delete1Mode | DeleteAllMode
instance Show KaniMode where
  show NewIdMode = "createNewId"
  show GetDataMode = "getData"
  show UpdateMode = "mydataUpdate"
  show LogSoonMode = "loggetSoon"
  show LogCometMode = "loggetComet"
  show EnterMode = "enter"
  show ExitMode = "exit"
  show WriteMode = "logWrite"
  show Delete1Mode = "deleteLogIndivisual"
  show DeleteAllMode = "deleteLog"

data KaniRequest = KaniRequest
  { reqMode :: Maybe KaniMode
  , reqRoomId :: String
  , reqName :: String
  , reqId :: Maybe String
  , reqMessage :: Maybe String
  , reqTrip :: Maybe String
  , reqPassword :: Maybe String
  , reqColor :: Maybe (Word8,Word8,Word8)
  , reqBold :: Bool
  , reqItalic :: Bool
  , reqStrike :: Bool
  , reqKanappsIcon :: Maybe String
  , reqSize :: TextSize
  , reqLogno :: Maybe Integer
  , reqUnderscore :: Maybe Integer
  }

kaniQuery :: String -> KaniRequest -> String
kaniQuery url req = url ++ "?" ++ kaniQuery' req

kaniQuery' :: KaniRequest -> String
kaniQuery' req
  =  normalGet True reqRoomId "roomid"
  ++ maybeGet  False (fmap show . reqMode) "mode"
  ++ normalGet False reqName "name"
  ++ maybeGet  False reqId "id"
  ++ maybeGet  False reqTrip "trip"
  ++ maybeGet  False reqMessage "message"
  ++ maybeGet  False (fmap showColor . reqColor) "color"
  ++ normalGet False showReqUserOption "user_option"
  ++ maybeGet  False (fmap show . reqLogno) "startLogNo"
  where
    showColor (r,g,b) = '#' : (showHex r $ showHex g $ showHex b "")
    showBool True = "true"
    showBool False = "false"
    showReqUserOption _ = normalGet True (showBool . reqBold) "bold"
      ++ normalGet False (showBool . reqItalic) "italic"
      ++ normalGet False (showBool . reqStrike) "strike"
      ++ normalGet False (show . reqSize) "size"
      ++ maybeGet False reqKanappsIcon "kanappsIcon"
    query b name val = (if b then "" else "&") ++ name ++ "=" ++ val
    normalGet b f name = query b name . urlEncode $ f req
    maybeGet b f name = maybe "" id $ query b name . urlEncode <$> f req

defaultRequest :: String -> String -> KaniRequest
defaultRequest name roomId = KaniRequest
  { reqMode = Nothing
  , reqRoomId = roomId
  , reqName = name
  , reqId = Nothing
  , reqMessage = Nothing
  , reqTrip = Nothing
  , reqPassword = Nothing
  , reqColor = Nothing
  , reqBold = False
  , reqItalic = False
  , reqStrike = False
  , reqKanappsIcon = Nothing
  , reqSize = Normal
  , reqLogno = Nothing
  , reqUnderscore = Nothing
  }

-- Maybeを増やす？
data KaniResponse = KaniResponse
  { resMemberStatusAll :: Integer
  , resMemberStatusEnter :: Integer
  , resMemberStatusIncludeAll :: Maybe Bool
  , resMemberStatusMemdata :: Maybe [Memdata]
  , resMydata :: Memdata
  , resRoomId :: String
  , resLog :: Maybe [KaniLog]
  , resSessionAlive :: Bool
  , resSessionId :: String
  , resStatusCode :: Int
  , resStatusCodeString :: String
  } deriving Show

instance FromJSON KaniResponse where
  parseJSON (Object x) = KaniResponse 
    <$> (x .: "memberStatus" >>= \y -> y .: "all")
    <*> (x .: "memberStatus" >>= \y -> y .: "enter")
    <*> (x .: "memberStatus" >>= \y -> y .:? "includeAll")
    <*> (x .: "memberStatus" >>= \y -> y .:? "memdata")
    <*> (x .: "mydata")
    <*> (x .: "room" >>= \y -> y .: "roomid")
    <*> (x .:? "chatlog")
    <*> (x .: "session" >>= \y -> y .: "alive")
    <*> (x .: "session" >>= \y -> y .: "id")
    <*> (x .: "status" >>= \y -> y .: "code")
    <*> (x .: "status" >>= \y -> y .: "codeString")
  parseJSON _ = empty

data KaniLog = KaniLog
  { logColor :: Maybe String
  , logIp :: String
  , logLogno :: Integer
  , logLogtime :: Integer
  , logMessage :: String
  , logName :: String
  , logServOption :: Maybe String
  , logUserOption :: Maybe String
  } deriving Show

instance FromJSON KaniLog where
  parseJSON (Object x) = KaniLog
    <$> x .:? "color"
    <*> x .: "ip"
    <*> x .: "logno"
    <*> x .: "logtime"
    <*> x .: "message"
    <*> x .: "name"
    <*> x .:? "serv_option"
    <*> x .:? "user_option"
  parseJSON _ = mempty

data Memdata = Memdata
  { memElapsedLastChatTime :: Integer
  , memExpires :: Integer
  , memIp :: String
  , memName :: Maybe String
  , memOpenid :: String
  , memServOption :: String
  , memUserOption :: Maybe String
  } deriving Show

instance FromJSON Memdata where
  parseJSON (Object x) = Memdata
    <$> x .: "elapsedLastChatTime"
    <*> x .: "expires"
    <*> x .: "ip"
    <*> x .:? "name"
    <*> x .: "openid"
    <*> x .: "serv_option"
    <*> x .:? "user_option"
  parseJSON _ = empty

data KaniConfig = KaniConfig
  { kaniProxy :: Maybe Proxy
  , kaniFun :: KaniRequest -> KaniRequest
  }

defaultConfig :: KaniConfig
defaultConfig = KaniConfig
  { kaniProxy = Nothing
  , kaniFun = id
  }
