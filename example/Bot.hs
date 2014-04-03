import Network.MaroKani
import Network.MaroKani.Bot
import qualified Language.Calc as Calc
import qualified Language.MaroKani as MaroKani

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Applicative

actions :: [Script]
actions =
  [ colonsEnd (not <$> isSelf) everyone ["m","cal"] $ \code -> do
    let s = either to2byteSpace (("res: " ++) . show) $ Calc.run code
    say_ s
  , colonsEnd (not <$> isSelf) everyone ["m","m"] $ \code -> do
    s <- liftIO $ catch (MaroKani.run (Just 2) (Just 512) code) tostr
    let s' = if null s then "(empty)" else s
    liftIO $ putStrLn s'
    say_ s'
  , colonsEnd (not <$> isSelf) everyone ["m","parse"] $ \code -> do
    s <- liftIO $ catch (show <$> MaroKani.parseIO' (Just 2) (Just 512) code) tostr
    say_ s
  , colonsSep (not <$> isSelf) isOwner ["m","cmd"] "exit" exit_
  , colonsSep (not <$> isSelf) isOwner ["m","cmd"] "enter" enter_
  , colonsEnd (not <$> isSelf) everyone ["m","cmd","update_name"] $ \name -> do
    updateName name
    exit_
    enter_
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

reacts :: KaniResponse -> Kani ()
reacts = mkReacts actions

bot :: String -> String -> String -> IO ()
bot name roomId trip = do
  let config = defaultConfig
  let req = (defaultRequest name roomId) { reqTrip = Just trip }
  runKani config req $ do
    res <- newId
    liftIO $ putStrLn $ resSessionId res
    soon_
    let loop = do
          comet >>= reacts
          loop
    loop

botId :: String -> String -> String -> String -> IO ()
botId name roomId trip sId = do
  let config = defaultConfig
  let req = (defaultRequest name roomId) { reqTrip = Just trip, reqId = Just sId }
  runKani config req $ do
    liftIO $ putStrLn sId
    soon_
    let loop = do
          comet >>= reacts
          loop
    loop

main :: IO ()
main = do
  t <- getLine
  s <- getLine
  if null s
    then bot "λまろろろ" "petcwiki" t
    else botId "λまろろろ" "petcwiki" t s
