module Network.MaroKani.Types
( KaniMode(..)
, KaniRequest(..)
, defaultRequest
, kaniQuery
, kaniQuery'
, KaniResponse(..)
, KaniLog(..)
, Memdata(..)
, KaniAnnounce(..)
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
  show Delete1Mode = "deleteLogIndividual"
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
  , reqStartLogNo :: Maybe Integer
  , reqLogNo :: Maybe Integer
  , reqUnderscore :: Maybe Integer
  } deriving (Show)

kaniQuery :: String -> KaniRequest -> String
kaniQuery url req = url ++ "?" ++ kaniQuery' req

kaniQuery' :: KaniRequest -> String
kaniQuery' req =  pureGet True reqRoomId "roomid"
  ++ maybeGet False (fmap show . reqMode) "mode"
  ++ pureGet  False reqName "name"
  ++ maybeGet False reqId "id"
  ++ maybeGet False reqTrip "trip"
  ++ maybeGet False reqMessage "message"
  ++ maybeGet False (fmap showColor . reqColor) "color"
  ++ pureGet  False showReqUserOption "user_option"
  ++ maybeGet False (fmap show . reqStartLogNo) "startLogNo"
  ++ maybeGet False (fmap show . reqLogNo) "logno"
  where
    showColor (r,g,b) = '#' : (showHex r $ showHex g $ showHex b "")
    showBool True = "true"
    showBool False = "false"
    showReqUserOption _ = pureGet True (showBool . reqBold) "bold"
      ++ pureGet  False (showBool . reqItalic) "italic"
      ++ pureGet  False (showBool . reqStrike) "strike"
      ++ pureGet  False (show . reqSize) "size"
      ++ maybeGet False reqKanappsIcon "kanappsIcon"
    query b name val = (if b then "" else "&") ++ name ++ "=" ++ val
    pureGet b f name = query b name . urlEncode $ f req
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
  , reqStartLogNo = Nothing
  , reqLogNo = Nothing
  , reqUnderscore = Nothing
  }

data KaniResponse = KaniResponse
  { resAnounce :: Maybe [KaniAnnounce]
  , resMemberStatusAll :: Maybe Integer
  , resMemberStatusEnter :: Maybe Integer
  , resMemberStatusIncludeAll :: Maybe Bool
  , resMemberStatusMemdata :: Maybe [Memdata]
  , resMydata :: Memdata
  , resRoomId :: String
  , resLog :: Maybe [KaniLog]
  , resSessionAlive :: Bool
  , resSessionId :: String
  , resStatusCode :: Int
  , resStatusCodeString :: String
  } deriving (Show)

instance FromJSON KaniResponse where
  parseJSON (Object x) = do
    memberStatus <- x .:? "memberStatus"
    let Nothing .:: _ = return Nothing
        Just o .:: name = Just <$> (o .: name)
    let Nothing .::? _ = return Nothing
        Just o .::? name = o .:? name
    KaniResponse
      <$> (x .:? "announce")
      <*> (memberStatus .:: "all")
      <*> (memberStatus .:: "enter")
      <*> (memberStatus .::? "includeAll")
      <*> (memberStatus .::? "memdata")
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
  } deriving (Show)

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
  } deriving (Show)

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

data KaniAnnounce = KaniAnnounce
  { annId :: String
  , annMessage :: String
  , annTitle :: String
  , annType :: String
  } deriving (Show)

instance FromJSON KaniAnnounce where
  parseJSON (Object x) = KaniAnnounce
    <$> x .: "id"
    <*> x .: "message"
    <*> x .: "title"
    <*> x .: "type"
  parseJSON _ = empty

data KaniConfig = KaniConfig
  { kaniProxy :: Maybe Proxy
  , kaniFun :: KaniRequest -> KaniRequest
  , kaniManager :: Manager
  }

defaultConfig :: IO KaniConfig
defaultConfig = do
  man <- newManager conduitManagerSettings
  return $ KaniConfig
    { kaniProxy = Nothing
    , kaniFun = id
    , kaniManager = man
    }
