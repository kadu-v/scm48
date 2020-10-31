{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}

module Eval where

import Control.Monad.Error
import Control.Monad.ST
import Data.STRef
import Error
import Syntax

--
eval :: Env s -> LispVal -> STThrowsError s LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, conseq, alt]) =
  do
    result <- eval env pred
    case result of
      Bool True -> eval env conseq
      Bool False -> eval env alt
eval env (List [Atom "set!", Atom var, form]) = eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) = eval env form >>= defineVar env var
eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badform = throwError $ BadSpecialForm "Unrecognized special form" badform

--
apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args =
  maybe (throwError $ NotFunction "Unrecognised primitive function args" func) ($ args) (lookup func primitives)

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
    ("string>=?", strBoolBinop (>=)),
    ("car", car),
    ("cdr", cdr),
    ("cons", cons),
    ("eqv", eqv),
    ("equal", equal)
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
   in if null parsed
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

--
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

--
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [xs] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

--
cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x, y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

--
eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && (all eqvPair $ zip arg1 arg2)
  where
    eqvPair (x1, x2) = case eqv [x1, x2] of
      Left _ -> False
      Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

--
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

--
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
  do
    unpacked1 <- unpacker arg1
    unpacked2 <- unpacker arg2
    return $ unpacked1 == unpacked2
    `catchError` (const $ return False)

--
equal :: [LispVal] -> ThrowsError LispVal
equal [arg1, arg2] = do
  primitiveEquals <-
    liftM or $
      mapM
        (unpackEquals arg1 arg2)
        [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

{-
TODO: IORef をSTRefに置き換えて，unittestが走るようにする．
-}
--
type Env s = STRef s [(String, STRef s LispVal)]

--
type STThrowsError s = ErrorT LispError (ST s)

--
liftThrows :: ThrowsError a -> STThrowsError s a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

--
runSTThrows :: STThrowsError s String -> ST s String
runSTThrows action = runErrorT (trapError action) >>= return . extractValue

--
nullEnv :: ST s (Env s)
nullEnv = newSTRef []

--
isBound :: Env s -> String -> ST s Bool
isBound envRef var = readSTRef envRef >>= return . maybe False (const True) . lookup var

--
getVar :: Env s -> String -> STThrowsError s LispVal
getVar envRef var = do
  env <- lift $ readSTRef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable: " var)
    (lift . readSTRef)
    (lookup var env)

--
setVar :: Env s -> String -> LispVal -> STThrowsError s LispVal
setVar envRef var value = do
  env <- lift $ readSTRef envRef
  maybe
    (throwError $ UnboundVar "Getting an unbound variable: " var)
    (lift . (flip writeSTRef value))
    (lookup var env)
  return value

--
defineVar :: Env s -> String -> LispVal -> STThrowsError s LispVal
defineVar envRef var value = do
  alreadyDefined <- lift $ isBound envRef var
  if alreadyDefined
    then setVar envRef var value
    else lift $ do
      valueRef <- newSTRef value
      env <- readSTRef envRef
      writeSTRef envRef ((var, valueRef) : env)
      return value

--
bindVars :: Env s -> [(String, LispVal)] -> ST s (Env s)
bindVars envRef bindings = readSTRef envRef >>= extendEnv bindings >>= newSTRef
  where
    extendEnv bindings env = liftM (\e -> e ++ env) (mapM addBindings bindings)
    addBindings (var, value) = do
      ref <- newSTRef value
      return (var, ref)
