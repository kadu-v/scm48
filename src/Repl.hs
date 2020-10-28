module Repl where

import Control.Monad
import Error
import Eval
import Parser
import System.IO

--
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

--
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

--
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

--
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

--
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

--
runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

--
runRepl :: IO ()
runRepl = nullEnv >>= until_ (\x -> x == "quit" || x == ":q") (readPrompt "scm48> ") . evalAndPrint
