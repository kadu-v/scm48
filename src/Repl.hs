{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Repl where

import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans
--import Control.Monad.Trans.ST
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
evalString :: Env s -> String -> ST s String
evalString env expr = runSTThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env

--
data STIO s a = STT s IO a

--
--evalAndPrint :: (forall s. Env s) -> String -> STIO String
--evalAndPrint env expr = let x = runST $ evalString env expr in x

--
-- until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
-- until_ pred prompt action = do
--   result <- prompt
--   if pred result
--     then return ()
--     else action result >> until_ pred prompt action

-- --
-- runOne :: String -> IO ()
-- runOne expr = nullEnv >>= flip evalAndPrint expr

-- --
-- -- runRepl :: IO ()
-- runRepl = nullEnv >>= until_ (\x -> x == "quit" || x == ":q") (readPrompt "scm48> ") . evalAndPrint
