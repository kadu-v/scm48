module Syntax where

import Control.Monad.Error
import Data.IORef
import System.IO
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

--
type Env = IORef [(String, IORef LispVal)]

--
data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
  | Func {params :: [String], vararg :: Maybe String, body :: [LispVal], closure :: Env}
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | Port Handle

--

instance Show LispVal where
  show = showVal

--
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " ." ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = vararg, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args)
    ++ ( case vararg of
           Nothing -> ""
           Just arg -> " . " ++ arg
       )
    ++ ")..)"
showVal (Port _) = "<IO Port>"
showVal (IOFunc _) = "<IO primitive>"

--
unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

--
data LispError
  = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

-- showError
showError :: LispError -> String
showError (UnboundVar message var) = message ++ ": " ++ var
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values" ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse err at " ++ show parseErr

--
instance Show LispError where
  show = showError

--
instance Error LispError where
  noMsg = Default "An error has occured"
  strMsg = Default

--
type ThrowsError = Either LispError

--
trapError :: (MonadError a m, Show a) => m String -> m String
trapError action = catchError action (return . show)

--
extractValue :: ThrowsError a -> a
extractValue (Right val) = val

--
type IOThrowsError = ErrorT LispError IO

--
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

--
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue
