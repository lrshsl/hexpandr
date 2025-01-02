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

whitespace :: Parser ()
whitespace = silent . many1 . choice $ map char " \t"

alpha :: Parser Char
alpha = force choice $ map char (['a' .. 'z'] ++ ['A' .. 'Z'])

digit :: Parser Char
digit = force choice $ map char ['0' .. '9']

alphanum :: Parser Char
alphanum = alpha <|> digit

ident :: Parser [Char]
ident = (alpha <|> char '_') `followedBy` many (alpha <|> digit <|> char '_')

int :: Parser Int
int = do
  x <- many1 digit
  return (read x :: Int)

float :: Parser Double
float = do
  text <- many1 digit `followedBy` char '.' `followedByFlat` many digit
  return (read text :: Double)
