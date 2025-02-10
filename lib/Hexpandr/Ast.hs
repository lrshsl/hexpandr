module Hexpandr.Ast where

import Hexpandr.Parser
import Hexpandr.PrimitiveParsers
import Hexpandr.Tokens

parseFile :: Parser [TopLevel]
parseFile = many parseMapping

data TopLevel
  = Mapping AstIdent [Arg] String
  | Expr
  deriving (Eq, Show)

keyword :: String -> Parser ()
keyword s = silent $ optional whitespace >> exactly s

parseMapping :: Parser TopLevel
parseMapping = do
  n <- keyword "df" >> whitespace >> parseIdent
  a <- many1 (whitespace >> parseArg)

  _ <- keyword "->" >> whitespace

  s <- strDelimiter >> many (anyCharBut "\'")
  _ <- strDelimiter

  return $ Mapping n a s
  where
    strDelimiter = silent (char '\'')

data Arg =
  IdentArg AstIdent
  | IntArg AstInt
  deriving (Eq, Show)

parseArg :: Parser Arg
parseArg =
  fmap IdentArg parseIdent
  <|> fmap IntArg parseInt

newtype AstIdent = AstIdent String
  deriving (Eq, Show)

parseIdent :: Parser AstIdent
parseIdent = AstIdent <$> ((alpha <|> char '_') `followedBy` many (alphanum <|> char '_'))

newtype AstInt = AstInt Int
  deriving (Eq, Show)

parseInt :: Parser AstInt
parseInt = fmap AstInt $ read <$> many1 digit

