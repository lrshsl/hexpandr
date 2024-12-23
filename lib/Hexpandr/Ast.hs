module Hexpandr.Ast where

import Hexpandr.Parser
import Hexpandr.PrimitiveParsers
import Hexpandr.Tokens

parseFile :: Parser [TopLevel]
parseFile = many parseMapping

data TopLevel =
  Mapping AstIdent [Arg] String
  deriving (Show)

parseMapping :: Parser TopLevel
parseMapping = do
  _ <- whitespace >> exactly "df"
  n <- whitespace >> parseIdent
  t <- whitespace >> parseArg

  _ <- whitespace >> exactly "->"

  _ <- whitespace >> char '\''
  s <- many $ anyCharBut "\'"
  _ <- char '\''
  return $ Mapping n [t] s

data Arg =
  IdentArg AstIdent
  | IntArg AstInt
  deriving (Show)

parseArg :: Parser Arg
parseArg =
  fmap IdentArg parseIdent
  <|> fmap IntArg parseInt

newtype AstIdent = AstIdent String
  deriving (Show)
newtype AstInt = AstInt Int
  deriving (Show)

parseIdent :: Parser AstIdent
parseIdent = AstIdent <$> many alpha

parseInt :: Parser AstInt
parseInt = fmap AstInt $ read <$> many digit

