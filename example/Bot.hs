{-# LANGUAGE TupleSections #-}

import Network.MaroKani
import Network.MaroKani.Bot
import qualified Language.Calc as Calc
import qualified Language.MaroKani as MaroKani
import qualified Language.MaroKani.Utils as Utils

import Control.Monad.Trans
import Control.Monad.Catch
import Control.Applicative
import System.Environment (getArgs)

kaniLib :: Kani Utils.EnvC
kaniLib = do
  enterIO <- kaniIO enter_
  exitIO <- kaniIO exit_
  let updateNameF name v = liftIO (Utils.value2string name v) >>= updateName >> exit_ >> enter_
  updateNameIO <- kaniIO2 updateNameF
  let updateIconF name v = liftIO (Utils.value2string name v) >>= updateIcon >> update_
  updateIconIO <- kaniIO2 updateIconF
  deleteAllIO <- kaniIO deleteAll_
  let deleteF name v = liftIO (Utils.value2integer name v) >>= delete_
  deleteIO <- kaniIO2 deleteF
  return $ Utils.fromList
    [ Utils.mkIOValueVoid (const enterIO) "enter"
    , Utils.mkIOValueVoid (const exitIO) "exit"
    , Utils.mk1ArgVoid updateNameIO "update_name"
    , Utils.mk1ArgVoid updateIconIO "update_icon"
    , Utils.mkIOValueVoid (const deleteAllIO) "deleteAll"
    , Utils.mk1ArgVoid deleteIO "delete"
    ]

actions :: [Script]
actions =
  [ colonsEnd (not <$> isSelf) everyone ["m","cal"] $ \code -> do
    let s = either to2byteSpace (("res: " ++) . show) $ Calc.run code
    say_ s
  , colonsEnd (not <$> isSelf) everyone ["m","m"] $ \code -> do
    lib <- kaniLib
    s <- liftIO $ handle tostr (MaroKani.run' lib (Just 2) (Just 512) code)
    putAndSay $ ifEmpty s
  , colonsEnd (not <$> isSelf) everyone ["m","parse"] $ \code -> do
    s <- liftIO $ handle tostr (show <$> MaroKani.parse (Just 2) (Just 512) code)
    say_ s
  , colonsEnd (not <$> isSelf) everyone ["m","time"] $ \code -> do
    (s,t) <- liftIO $ handle (\e -> (,Nothing) <$> tostr e)
                ((\(s,t) -> (s, Just t)) <$> MaroKani.bench (Just 2) (Just 512) code)
    putAndSay $ ifEmpty s
    let showMS t' = "time: " ++ shows (fromRational $ toRational t' * 1000 :: Double) "ms"
    maybe (return ()) (putAndSay . showMS) t
  , colonsSep (not <$> isSelf) isOwner ["m","cmd"] "exit" exit_
  , colonsSep (not <$> isSelf) isOwner ["m","cmd"] "enter" enter_
  , colonsEnd (not <$> isSelf) everyone ["m","cmd","update_name"] $ \name ->
    updateName name >> exit_ >> enter_
  , colonsEnd (not <$> isSelf) everyone ["m","cmd","update_icon"] $ \name ->
    updateIcon name >> update_
  , colonsSep (not <$> isSelf) isOwner ["m","cmd","delete"] "all" deleteAll_
  , colonsEnd (not <$> isSelf) everyone ["m","cmd","delete"] $ \n ->
    case reads n of
      [(n',"")] -> delete_ n'
      _ -> say_ "parser error"
  , colonsEnd (not <$> isSelf) everyone ["m","cmd"] $ \name -> say_ $ name ++ "なんて、あるわけない"
  ]
  where
    to2byteSpace (' ':' ':' ':' ':xs) = "　 " ++ to2byteSpace xs
    to2byteSpace (' ':' ':xs) = "　" ++ to2byteSpace xs
    to2byteSpace (x:xs) = x : to2byteSpace xs
    to2byteSpace "" = ""
    tostr :: MaroKani.MaroKaniException -> IO String
    tostr = return . to2byteSpace . show
    ifEmpty "" = "(empty)"
    ifEmpty s = s
    putAndSay s = liftIO (putStrLn s) >> say_ s

reacts :: KaniResponse -> Kani ()
reacts = mkReacts actions

bot :: String -> String -> String -> IO ()
bot name roomId trip = do
  config <- defaultConfig
  let req = (defaultRequest name roomId) { reqTrip = Just trip }
  runKani config req $ flip finally exit_ $ do
    res <- newId
    liftIO $ putStrLn $ resSessionId res
    soon_
    let loop = do
          comet >>= reacts
          loop
    loop

botId :: String -> String -> String -> String -> IO ()
botId name roomId trip sId = do
  config <- defaultConfig
  let req = (defaultRequest name roomId) { reqTrip = Just trip, reqId = Just sId }
  runKani config req $ flip finally exit_ $ do
    liftIO $ putStrLn "start.."
    soon_
    let loop = do
          comet >>= reacts
          loop
    loop

parseArgs :: [String] -> Maybe (Maybe String, Maybe String, Maybe String, Maybe String)
parseArgs = go (Nothing,Nothing,Nothing,Nothing)
  where
    go acc [] = Just acc
    go (_,r,t,s) ("-n":n:xs) = go (Just n,r,t,s) xs
    go (n,_,t,s) ("-r":r:xs) = go (n,Just r,t,s) xs
    go (n,r,_,s) ("-t":t:xs) = go (n,r,Just t,s) xs
    go (n,r,t,_) ("-s":s:xs) = go (n,r,t,Just s) xs
    go _ _ = Nothing

main :: IO ()
main = do
  args <- parseArgs <$> getArgs
  case args of
    Just (Just n, Just r, mt, Just s) -> botId n r (maybe "" id mt) s
    Just (Just n, Just r, mt, Nothing) -> bot n r (maybe "" id mt)
    _ -> putStrLn "Usage: marokanibot -n <name> -r <roomid> [-t <trip>] [-s <sessionid>]"
