module Main where

import Eval
import Lib
import Parser
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head