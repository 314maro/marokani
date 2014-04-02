module Language.MaroKani
( run
, eval
, eval'
, parse
, parseIO
, MaroKaniException(..)
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

parseIO :: String -> IO [Expr]
parseIO code = case parse code of
  Failure doc -> throwM $ ParserError doc
  Success es -> return es

limit :: Int -> String -> IO String
limit i _ | i < 0 = throwM StringTooLong
limit _ [] = return []
limit i (x:xs) = (x :) <$> limit (i-1) xs

run :: Maybe Int -> Maybe Int -> String -> IO String
run time len code = do
  exprs <- parseIO code
  env <- newEnv
  o <- atomically $ newTVar id
  result <- case time of
    Nothing -> Right <$> eval' env o exprs
    Just t -> threadDelay (t*1000*1000) `race` eval' env o exprs
  case result of
    Left _ -> throwM TimeOver
    Right _ -> do
      s <- atomically $ readTVar o <*> pure ""
      case len of
        Nothing -> return s
        Just l -> limit l s
