module Main where

data Result ok err = Ok ok | Err err
    deriving (Eq, Ord, Read, Show)

data ParseError = ParseError { expected :: String, found :: String }
    deriving (Eq, Show)

type ParseResult = Result (String, String) ParseError

type Parser = String -> ParseResult

anyChar :: Parser
anyChar = \input -> case input of

  (x:xs) -> (Ok ([x], xs))

  []     -> Err $ ParseError { expected = "any character", found = "<EOI>" }


eof :: Parser
eof = \input -> case input of

  []    -> Ok ([], [])

  (c:_) -> Err $ ParseError { expected = "<EOI>", found = [c] }


class Exactly x where
    exactly :: x -> Parser

instance Exactly Char where
    exactly ch = \input -> case input of
        (c:rst)
            | c == ch   -> Ok ([ch], rst)
            | otherwise -> Err $ ParseError { expected = [ch], found = [c] }


instance Exactly String where
    exactly target = \input ->
        case splitAt (length target) input of
            (prefix, rst)
                | prefix == target -> Ok (prefix, rst)  -- Matched the string
                | otherwise        -> Err $ ParseError { expected = target, found = prefix }
            (_, _) -> Err $ ParseError { expected = target, found = "<EOI>" }  -- Not enough input


andThen :: Parser -> Parser -> Parser
a `andThen` b = \input ->
    case a input of
        Ok (s, rst) -> case b rst of
            Ok (s_b, rst_b) -> Ok (s ++ s_b, rst_b)
            err -> err
        err -> err


orElse :: Parser -> Parser -> Parser
a `orElse` b = \input ->
    case a input of
        Err _ -> b input
        ok -> ok


many :: Parser -> Parser
many p = \input ->
    case p input of
        Ok (s, rst) -> 
            case many p rst of
                Ok (s_b, rst_b) -> Ok (s ++ s_b, rst_b)
                Err _ -> Ok (s, input)
        Err _ -> Ok ([], input)


main :: IO ()
main = do
    let result1 = eof ""
    print result1  -- Output: Ok []
    
    let result2 = anyChar "Hello"
    print result2

    let result3 = eof "Hello"
    print result3

    let result4 = (anyChar `andThen` anyChar) "Hello"
    print result4

    let result5 = (many anyChar `andThen` eof) "Hello"
    print result5

    let result6 = (exactly '4') "Hello"
    print result6

    let result7 = (exactly "Hel") "Hello"
    print result7


-- vim: et ts=4 sts=4 sw=4
