module Main where

import Repl
import System.Environment

main :: IO ()
main =
  do
    args <- getArgs
    case null args of
      True -> runRepl
      False -> runOne $ args