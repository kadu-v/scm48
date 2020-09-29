module Main where

import Eval
import Lib
import Parser
import System.Environment

main :: IO ()
main =
  do
    x <- getArgs
    let y = (readExpr . head) x
    case y of
      Left x -> print x
      Right x -> print $ eval x