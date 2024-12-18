module Hexpandr where

import Control.DeepSeq (force)


------------------
-- Parser types --
------------------

data Result ok err = Ok ok | Err err
    deriving (Eq, Ord, Read, Show)

data ParseError = ParseError { expected :: String, found :: String }
    deriving (Eq, Show)

type ParseResult = Result (String, String) ParseError

type Parser = String -> ParseResult


------------------
-- Base parsers --
------------------

okParser :: Parser
okParser i = Ok ([], i)


anyChar :: Parser
anyChar i = case i of

  (x:xs) -> Ok ([x], xs)

  []     -> Err $ ParseError { expected = "any character", found = "<EOI>" }


eof :: Parser
eof i = case i of

  []    -> Ok ([], [])

  (c:_) -> Err $ ParseError { expected = "<EOI>", found = [c] }


chr :: Char -> Parser
chr c i = case i of
    (x:rst)
        | x == c    -> Ok ([x], rst)
        | otherwise -> Err $ ParseError { expected = [c], found = [x] }
    [] -> Err $ ParseError { expected = [c], found = "<EOF>" }


exactly :: String -> Parser
exactly s i =
    case splitAt (length s) i of
        (prefix, rst)
            | prefix == s -> Ok (prefix, rst)
            | otherwise   -> Err $ ParseError { expected = s, found = prefix }


digit :: Parser
digit = force oneOf (map chr "0123456789")

alpha :: Parser
alpha = force oneOf (map chr (['a' .. 'z'] ++ ['A' .. 'Z']))

alphanum :: Parser
alphanum = alpha `orElse` digit

defaultIdent :: Parser
defaultIdent = (alpha `orElse` chr '_') `andThen` many (alphanum `orElse` chr '_')


------------------------
-- Parser combinators --
------------------------

oneOf :: [Parser] -> Parser
oneOf = foldl1 orElse


optional :: Parser -> Parser
optional p = p `orElse` okParser


andThen :: Parser -> Parser -> Parser
a `andThen` b = \i ->
    case a i of
        Ok (s, rst) -> case b rst of
            Ok (s_b, rst_b) -> Ok (s ++ s_b, rst_b)
            err -> err
        err -> err


orElse :: Parser -> Parser -> Parser
a `orElse` b = \i ->
    case a i of
        Err _ -> b i
        ok -> ok


many :: Parser -> Parser
many p i =
    case p i of
        Ok (s, rst) ->
            case many p rst of
                Ok (s_b, rst_b) -> Ok (s ++ s_b, rst_b)
                Err _ -> Ok (s, i)
        Err _ -> Ok ([], i)

many1 :: Parser -> Parser
many1 p = p `andThen` many p

sepByStrict :: Parser -> Parser -> Parser
e `sepByStrict` s = e `andThen` ((s `andThen` sepBy e s) `orElse` okParser)

sepBy :: Parser -> Parser -> Parser
e `sepBy` s = e `andThen` ((s `andThen` optional (sepBy e s)) `orElse` okParser)


main :: IO ()
main = do putStrLn "done"


-- vim: et ts=4 sts=4 sw=4
