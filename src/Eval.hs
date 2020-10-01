module Eval where

import Control.Monad.Error (MonadError (throwError))
import Error
import Syntax

--
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quate", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badform = throwError $ BadSpecialForm "Unrecognized special form" badform

--
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognised primitive function args" func) ($ args) $ lookup func primitives

--
primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives =
  [ ("+", numericBinop (+)),
    ("-", numericBinop (-)),
    ("*", numericBinop (*)),
    ("/", numericBinop (div)),
    ("mod", numericBinop (mod)),
    ("quotient", numericBinop (quot)),
    ("remainder", numericBinop (rem)),
    ("=", numBoolBinop (==)),
    ("<", numBoolBinop (<)),
    (">", numBoolBinop (>)),
    ("/=", numBoolBinop (/=)),
    (">=", numBoolBinop (>=)),
    ("<=", numBoolBinop (<=)),
    ("&&", boolBoolBinop (&&)),
    ("||", boolBoolBinop (||)),
    ("string=?", strBoolBinop (==)),
    ("string<?", strBoolBinop (<)),
    ("string>?", strBoolBinop (>)),
    ("string<=?", strBoolBinop (<=)),
    ("string>=?", strBoolBinop (>=))
  ]

--
numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

--
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) =
  let parsed = reads n
   in if null n
        then throwError $ TypeMismatch "number" $ String n
        else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

--
boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args =
  if length args /= 2
    then throwError $ NumArgs 2 args
    else do
      left <- unpacker $ args !! 0
      right <- unpacker $ args !! 1
      return $ Bool $ left `op` right

--
numBoolBinop :: (Integer -> Integer -> Bool) -> [LispVal] -> ThrowsError LispVal
numBoolBinop = boolBinop unpackNum

--
strBoolBinop :: (String -> String -> Bool) -> [LispVal] -> ThrowsError LispVal
strBoolBinop = boolBinop unpackStr

--
boolBoolBinop :: (Bool -> Bool -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBoolBinop = boolBinop unpackBool

--
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "String" notString

--
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return $ b
unpackBool notBool = throwError $ TypeMismatch "Boolean" notBool