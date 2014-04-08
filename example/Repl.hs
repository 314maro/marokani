import Language.MaroKani
import Control.Monad.Catch

main :: IO ()
main = do
  putStr "> "
  s <- getLine
  if s == "quit"
    then return ()
    else do
      let f :: MaroKaniException -> IO String
          f = return . showColor
      r <- catch (run Nothing Nothing s) f
      putStrLn r
      main
