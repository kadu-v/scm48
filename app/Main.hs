module Main where

import Error
import Eval
import Lib
import Parser
import System.Environment

main :: IO ()
main =
  do
    args <- getArgs
    evaled <- return $ show <$> (readExpr (args !! 0) >>= eval)
    putStrLn $ extractValue $ trapError evaled