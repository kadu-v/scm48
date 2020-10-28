module Error where

import Control.Monad.Error
import Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

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