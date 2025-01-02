{-# LANGUAGE LambdaCase #-}

module Hexpandr.PrimitiveParsers
  ( eoi,
    char,
    anyChar,
    anyCharBut,
    exactly,
    module Hexpandr.FollowedByParser,
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

import Control.Monad qualified
import Hexpandr.Parser
import Hexpandr.FollowedByParser
import Hexpandr.Utils

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

anyChar :: Parser Char
anyChar = Parser $ \case
  (x : xs) -> Ok x xs
  [] -> Err "any character" "end of input"

anyCharBut :: [Char] -> Parser Char
anyCharBut excluded = Parser $ \case
  (x : xs)
    | x `notElem` excluded -> Ok x xs
    | otherwise -> Err ("any character except " ++ show excluded) [x]
  [] -> Err ("any character except " ++ show excluded) "end of input"

exactly :: String -> Parser String
exactly = traverse char

seqParsers :: [Parser a] -> Parser [a]
seqParsers [] = pure []
seqParsers (p : ps) = do
  r <- p
  rs <- seqParsers ps
  return $ r : rs

(<|>) :: Parser a -> Parser a -> Parser a
(Parser p1) <|> (Parser p2) = Parser $ \input ->
  case p1 input of
    Ok value rest -> Ok value rest
    Err _ _ -> p2 input

choice :: [Parser a] -> Parser a
choice = foldr1 (<|>)

optional :: Parser a -> Parser [a]
optional p = fmap (: []) p <|> pure []

silent :: Parser p -> Parser ()
silent = Control.Monad.void

many :: Parser a -> Parser [a]
many p = flatten . optional $ p `followedBy` many p

many1 :: Parser a -> Parser [a]
many1 p = p `followedBy` many p

sepBy :: Parser e -> Parser s -> Parser [e]
e `sepBy` s = flatten $ optional e `followedBy` many (silent s >> e)

sepBy1 :: Parser e -> Parser s -> Parser [e]
e `sepBy1` s = e `followedBy` (e `sepBy` s)
