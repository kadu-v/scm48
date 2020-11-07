module Repl where

import Control.Monad
import Eval
import Parser
import Syntax
import System.Directory
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
runOne :: [String] -> IO ()
runOne args = do
  --print args
  -- cwd <- getCurrentDirectory
  -- print cwd
  -- env <- primitiveBinding >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  env <- primitiveBinding >>= flip bindVars [("args", List $ map String $ drop 1 args)]
  (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)])) >>= hPutStrLn stderr

--
runRepl :: IO ()
runRepl = primitiveBinding >>= until_ (\x -> x == "quit" || x == ":q") (readPrompt "scm48> ") . evalAndPrint
