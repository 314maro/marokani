module Language.MaroKani
  ( run
  , run'
  , bench
  , parse
  , MaroKaniException(..)
  , showColor
  ) where

import Language.MaroKani.Types
import Language.MaroKani.Prim
import Language.MaroKani.Parser
import Language.MaroKani.Eval

import Control.Applicative
import Control.Monad.Catch
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.Time.Clock as Time

lenLimit :: Int -> String -> IO String
lenLimit i _ | i < 0 = throwM StringTooLong
lenLimit _ [] = return []
lenLimit i (x:xs) = (x :) <$> lenLimit (i-1) xs

timeLimitMaybe :: Maybe Int -> IO a -> IO a
timeLimitMaybe Nothing m = m
timeLimitMaybe (Just t) m = do
  result <- threadDelay (t*1000*1000) `race` m
  case result of
    Left _ -> throwM $ TimeOver t
    Right r -> return r

run :: Maybe Int -> Maybe Int -> String -> IO String
run = runF newEnv

run' :: EnvC -> Maybe Int -> Maybe Int -> String -> IO String
run' envc = runF (newEnv' envc)

runF :: IO Env -> Maybe Int -> Maybe Int -> String -> IO String
runF ioenv time len code = do
  o <- atomically $ newTVar id
  timeLimitMaybe time $ do
    exprs <- parseIO code
    env <- ioenv
    _ <- eval' env o exprs
    s <- atomically $ readTVar o <*> pure ""
    case len of
      Nothing -> return s
      Just l -> lenLimit l s

bench :: Maybe Int -> Maybe Int -> String -> IO (String, Time.NominalDiffTime)
bench time len code = do
  begin <- Time.getCurrentTime
  s <- run time len code
  end <- Time.getCurrentTime
  let t = Time.diffUTCTime end begin
  return (s,t)

parse :: Maybe Int -> Maybe Int -> String -> IO String
parse time len code = do
  s <- timeLimitMaybe time $ parseIO code
  case len of
    Nothing -> return $ show s
    Just l -> lenLimit l $ show s
