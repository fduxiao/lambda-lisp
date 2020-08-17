module Main where

import System.Console.Haskeline
import Repl

main :: IO ()
main = runInputT defaultSettings (loop emptyEnv)
    where
        loop :: Env -> InputT IO ()
        loop env = do
            minput <- getInputLine "user> "
            case minput of
                Nothing -> return ()
                Just input -> do 
                    (_, env') <- liftIO $ runRepl (repl input) env
                    loop env'
