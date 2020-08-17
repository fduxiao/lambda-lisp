module Repl(
    repl, 
    liftIO,
    Env,
    lastResult,
    emptyEnv,
    runRepl
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char(space)
import Control.Monad.IO.Class
import Control.Monad.Trans.State.Lazy

import Lambda

data Env = Env {lastResult :: Term, partial :: String}

emptyEnv :: Env
emptyEnv = Env {lastResult = Var "empty", partial=""}

data ReplLambda = None | Full Term | Partial String

replLambda :: Parser ReplLambda
replLambda = try (Full <$> term) <|> try (many space >> eof >> return None) 

data Input = Reduce (Maybe Term) | Rename V V (Maybe Term) | EmptyLine | Err String

parseInput :: String -> Input
parseInput "reduce" = Reduce Nothing
parseInput ('r':'e':'d':'u':'c':'e':' ':s) = case parse replLambda "stdin" s of
        Left e -> Err $ show e
        Right None -> Reduce Nothing
        Right (Full a) -> Reduce (Just a)

parseInput a = parseInput $ "reduce " ++ a
        

type Repl a = StateT Env IO a

runRepl :: Repl a -> Env -> IO (a, Env)
runRepl = runStateT

repl :: String -> Repl ()
repl line = case parseInput line of
    Reduce (Just a) -> let r = reduce a in do
        liftIO $ print r
        modify (\env -> env{lastResult = r})
    Reduce Nothing -> do
        t <- gets lastResult
        let r = reduce t
        liftIO $ print r
        modify (\env -> env{lastResult = r})
