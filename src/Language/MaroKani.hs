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

import Control.Monad.Catch
import Control.Concurrent.STM
import Control.Concurrent.Async

parseIO :: String -> IO [Expr]
parseIO code = case parse code of
  Failure doc -> throwM $ ParserError doc
  Success es -> return es

-- tchanやめよう
run :: String -> IO String
run code = do
  es <- parseIO code
  env <- newEnv
  chan <- atomically newTChan
  s <- atomically $ newTVar ""
  result <- observe s chan `race` eval' env chan es
  case result of
    Left _ -> throwM $ Default "長すぎ"
    Right _ -> atomically (readTVar s)
  where
    observe res chan =
      let loop i
            | i >= 512 = return ()
            | otherwise = do
              s <- atomically $ readTChan chan
              atomically $ modifyTVar res (s ++)
              loop (length s + i)
      in loop 0
