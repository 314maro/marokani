{-# LANGUAGE TupleSections #-}

import Network.MaroKani
import Network.MaroKani.Bot
import qualified Language.Calc as Calc
import qualified Language.MaroKani as MaroKani

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Applicative
import System.IO (hFlush,stdout)

-- kaniLib :: Kani EnvC
-- kaniLib = []

actions :: [Script]
actions =
  [ colonsEnd (not <$> isSelf) everyone ["m","cal"] $ \code -> do
    let s = either to2byteSpace (("res: " ++) . show) $ Calc.run code
    say_ s
  , colonsEnd (not <$> isSelf) everyone ["m","m"] $ \code -> do
    s <- liftIO $ handle tostr (MaroKani.run (Just 2) (Just 512) code)
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
  , colonsSep (not <$> isSelf) isOwner ["m","cmd","delete"] "all" $ void deleteAll
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

input :: String -> IO String
input s = do
  putStr s
  hFlush stdout
  t <- getLine
  return t

main :: IO ()
main = do
  t <- input "trip: "
  s <- input "session_id: "
  if null s
    then bot "λまろろろ" "petcwiki" t
    else botId "λまろろろ" "petcwiki" t s
