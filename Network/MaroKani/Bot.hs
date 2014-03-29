module Network.MaroKani.Bot (bot, botId) where

import Network.MaroKani
import qualified Language.Calc as Calc
import qualified Language.MaroKani as MaroKani

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Catch
import Control.Applicative
import Data.List (stripPrefix)

-- もっと柔軟に
data Script
  = ColonsEnd [String] (String -> Kani ())
  | ColonsSep [String] String (Kani ())

stripPrefixes :: [String] -> String -> Maybe String
stripPrefixes prefixes msg = foldM (\s pre -> stripPrefix pre s >>= stripColon) msg prefixes
  where
    stripColon (':':str) = Just str
    stripColon ('：':str) = Just str
    stripColon _ = Nothing

runScript :: String -> Script -> Maybe (Kani ())
runScript msg script = case script of
  ColonsEnd prefixes act -> act <$> prefixes `stripPrefixes` msg
  ColonsSep prefixes name act -> do
    s <- prefixes `stripPrefixes` msg
    guard (s == name)
    return act

mkReact :: [Script] -> KaniLog -> Kani ()
mkReact xs kaniLog = maybe (return ()) id
  $ msum $ map (runScript $ logMessage kaniLog) xs

mkReacts :: [Script] -> KaniResponse -> Kani ()
mkReacts acts res = maybe (return ()) id
  $ mapM_ (asyncKani . mkReact acts) <$> resLog res

actions :: [Script]
actions =
  [ ColonsEnd ["m","cal"] $ \code -> void $ do
    let s = either to2byteSpace (("res: " ++) . show) $ Calc.run code
    say s
  , ColonsEnd ["m","m"] $ \code -> void $ do
    let f :: MaroKani.MaroKaniException -> IO String
        f = return . to2byteSpace . show
    s <- liftIO $ catch (MaroKani.run code) f
    if null s then say "(empty)" else say s
  , ColonsEnd ["m","parse"] $ \code -> void $ do
    let f :: MaroKani.MaroKaniException -> IO String
        f = return . to2byteSpace . show
    s <- liftIO $ catch (show <$> MaroKani.parseIO code) f
    say s
  , ColonsSep ["m","cmd"] "exit" $ void exit
  , ColonsSep ["m","cmd"] "enter" $ void enter
  ]
  where
    to2byteSpace [] = []
    to2byteSpace (' ':' ':' ':xs) = "　 " ++ to2byteSpace xs
    to2byteSpace (' ':' ':xs) = "　" ++ to2byteSpace xs
    to2byteSpace (x:xs) = x : to2byteSpace xs

reacts :: KaniResponse -> Kani ()
reacts = mkReacts actions

bot :: String -> String -> IO ()
bot name roomId = do
  let config = defaultConfig
  let req = defaultRequest name roomId
  runKani config req $ do
    res <- newId
    liftIO $ putStrLn $ resSessionId res
    soon_
    let loop :: Kani ()
        loop = do
          comet >>= reacts
          loop
    loop
    return ()

botId :: String -> String -> String -> IO ()
botId sId name roomId = do
  let config = defaultConfig
  let req = (defaultRequest name roomId) { reqId = Just sId }
  runKani config req $ do
    liftIO $ putStrLn sId
    soon_
    let loop :: Kani ()
        loop = do
          comet >>= reacts
          loop
    loop
    return ()
