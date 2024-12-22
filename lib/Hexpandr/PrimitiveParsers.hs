{-# LANGUAGE LambdaCase #-}

module Hexpandr.PrimitiveParsers
  ( eoi,
    char,
    anyChar,
    exactly,
    andThen,
    followedBy,
    seqParsers,
    (<|>),
    choice,
    optional,
    silent,
    many,
    many1,
    sepBy,
    sepBy1,
  )
where

import Hexpandr.Parser
import qualified Control.Monad

-- Parser that consumes a single character
eoi :: Parser ()
eoi = Parser $ \case
  (x : _) -> Err "end of input" [x]
  [] -> Ok () []

char :: Char -> Parser Char
char c = Parser $ \case
  (x : xs)
    | x == c -> Ok c xs
    | otherwise -> Err [c] [x]
  [] -> Err [c] "end of input"

-- Parser that consumes any character
anyChar :: Parser Char
anyChar = Parser $ \case
  (x : xs) -> Ok x xs
  [] -> Err "any character" "end of input"

-- Parser that matches a specific string
exactly :: String -> Parser String
exactly = traverse char

-- Parser that applies one parser, then another, and combines the results
andThen :: Parser a -> Parser a -> Parser [a]
p1 `andThen` p2 = do
  a <- p1
  b <- p2
  return [a, b]

-- Parser that applies one parser, then another, and combines the results
followedBy :: Parser a -> Parser b -> Parser (a, b)
p1 `followedBy` p2 = do
  a <- p1
  b <- p2
  return (a, b)

seqParsers :: [Parser a] -> Parser [a]
seqParsers [] = pure []
seqParsers (p : ps) = do
  r <- p
  rs <- seqParsers ps
  return $ r : rs

-- Parser that tries one parser, and if it fails, tries another
(<|>) :: Parser a -> Parser a -> Parser a
(Parser p1) <|> (Parser p2) = Parser $ \input ->
  case p1 input of
    Ok value rest -> Ok value rest
    Err _ _ -> p2 input

choice :: [Parser a] -> Parser a
choice = foldr1 (<|>)

optional :: Parser a -> Parser [a]
optional p = fmap (:[]) p <|> pure []

silent :: Parser p -> Parser ()
silent = Control.Monad.void

many :: Parser a -> Parser [a]
many p = many1 p <|> pure []

many1 :: Parser a -> Parser [a]
many1 p = do
  first <- p
  rest <- many p
  return (first : rest)

sepBy :: Parser e -> Parser s -> Parser [e]
e `sepBy` s = (++) <$> optional e <*> many (silent s >> e)

sepBy1 :: Parser e -> Parser s -> Parser [e]
e `sepBy1` s = (:) <$> e <*> e `sepBy` s
