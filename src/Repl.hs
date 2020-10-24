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
evalString :: String -> IO String
evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

--
evalAndPrint :: String -> IO ()
evalAndPrint expr = evalString expr >>= putStrLn

--
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

--
runRepl :: IO ()
runRepl = until_ (\x -> x == "quit" || x == ":q") (readPrompt "scm48> ") evalAndPrint
