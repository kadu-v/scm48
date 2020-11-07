module Parser where

import Control.Monad.Except (MonadError (throwError))
import Syntax
  ( LispError (Parser),
    LispVal (Atom, Bool, DottedList, List, Number, String),
    ThrowsError,
  )
import Text.ParserCombinators.Parsec hiding (spaces)
import Text.ParserCombinators.Parsec.Error ()

--
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

--
spaces :: Parser ()
spaces = skipMany1 space

--
escapedChars :: Parser String
escapedChars = do
  char '\\'
  x <- oneOf "\\\"ntr"
  return $ case x of
    '\\' -> [x]
    '"' -> [x]
    't' -> "\t"
    'n' -> "\n"
    'r' -> "\r"

--
parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many $ many1 (noneOf "\"\\") <|> escapedChars
  char '"'
  return $ String $ concat x

--
parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first : rest
  return $ Atom atom

--
parseNumber :: Parser LispVal
parseNumber = Number . read <$> many1 digit

--
parseBool :: Parser LispVal
parseBool = do
  char '#'
  x <- oneOf "tf"
  return $ case x of
    't' -> Bool True
    'f' -> Bool False

--
parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr spaces

--
parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

--
parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

--
parseExpr :: Parser LispVal
parseExpr =
  parseAtom
    <|> parseNumber
    <|> parseString
    <|> parseBool
    <|> parseQuoted
    <|> do
      char '('
      x <- try parseList <|> parseDottedList
      char ')'
      return x

--
readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "scm48" input of
  Left err -> throwError (Parser err)
  Right val -> return val