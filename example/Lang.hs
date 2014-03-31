import Language.MaroKani
import Control.Monad.Catch

main :: IO ()
main = do
  s <- getLine
  let f :: MaroKaniException -> IO String
      f = return . show
  r <- catch (run s) f
  putStrLn r
  main
