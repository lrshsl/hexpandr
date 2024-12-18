module Hexpandr where

import Control.DeepSeq (force)


------------------
-- Parser types --
------------------

data Result ok err = Ok ok | Err err
    deriving (Eq, Ord, Read, Show)


type ParseResult = Result
    (String, String) -- Ok  (parsed, remaining)
    (String, String) -- Err (expected,   found)

type Parser = String -> ParseResult


------------------
-- Base parsers --
------------------

okParser :: Parser
okParser i = Ok ([], i)


anyChar :: Parser
anyChar i = case i of

  (x:xs) -> Ok ([x], xs)

  []     -> Err ("any character", "<EOI>")


eof :: Parser
eof i = case i of

  []    -> Ok ([], [])

  (c:_) -> Err ("<EOI>", [c])


chr :: Char -> Parser
chr c i = case i of
    (x:rst)
        | x == c    -> Ok ([x], rst)
        | otherwise -> Err ([c], [x])
    [] -> Err ([c], "<EOF>")


exactly :: String -> Parser
exactly s i =
    case splitAt (length s) i of
        (prefix, rst)
            | prefix == s -> Ok (prefix, rst)
            | otherwise   -> Err (s, prefix)


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
        Err (e, _) -> case b i of
            Err (e_b, f) -> Err (e ++ e_b, f)
            ok -> ok
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


sepBy :: Parser -> Parser -> Parser
e `sepBy` s = many ((e `andThen` s) `orElse` s) `andThen` optional e

sepBy1 :: Parser -> Parser -> Parser
e `sepBy1` s = e `andThen` (e `sepBy` s)


sepByStrict :: Parser -> Parser -> Parser
e `sepByStrict` s = optional (e `andThen` many (s `andThen` e))

sepByStrict1 :: Parser -> Parser -> Parser
e `sepByStrict1` s = e `andThen` (e `sepByStrict` s)


main :: IO ()
main = do putStrLn "done"


-- vim: et ts=4 sts=4 sw=4
