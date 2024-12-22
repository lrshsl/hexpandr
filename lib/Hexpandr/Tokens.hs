module Hexpandr.Tokens where

import Hexpandr.Parser
import Hexpandr.PrimitiveParsers

import Control.DeepSeq (force)

data Token
  = TokInt Int
  | TokFloat Double
  | TokIdent String
  | TokString String
  deriving (Show, Eq)

whitespace :: Parser String
whitespace = many $ char ' '

alpha :: Parser Char
alpha = force choice $ map char (['a' .. 'z'] ++ ['A' .. 'Z'])

digit :: Parser Char
digit = force choice $ map char ['0' .. '9']

ident :: Parser Token
ident = do
  x <- many1 alpha
  return $ TokIdent x

int :: Parser Token
int = do
  x <- many1 digit
  return . TokInt $ read x

float :: Parser Token
float = do
  ((a, dot), b) <- many1 digit `followedBy` char '.' `followedBy` many digit

  let combined = a ++ dot : b
   in return . TokFloat $ (read combined :: Double)

token :: Parser Token
token = do
  _ <- whitespace
  ident <|> float <|> int

