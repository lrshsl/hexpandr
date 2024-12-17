module Main where

data Result ok err = Ok ok | Err err
    deriving (Eq, Ord, Read, Show)

data ParseError = ParseError { expected :: String, found :: String }
    deriving (Eq, Show)

type ParseResult = Result (String, String) ParseError

type Parser = String -> ParseResult


okParser :: Parser
okParser = (\i -> Ok ([], i))


anyChar :: Parser
anyChar = \input -> case input of

  (x:xs) -> (Ok ([x], xs))

  []     -> Err $ ParseError { expected = "any character", found = "<EOI>" }


eof :: Parser
eof = \input -> case input of

  []    -> Ok ([], [])

  (c:_) -> Err $ ParseError { expected = "<EOI>", found = [c] }


oneOf :: [Parser] -> Parser
oneOf parsers = foldl orElse  (head parsers) (tail parsers)


digit :: Parser
digit = oneOf (map exactly "0123456789")


optional :: Parser -> Parser
optional p = p `orElse` okParser

class Exactly x where
    exactly :: x -> Parser

instance Exactly Char where
    exactly ch = \input -> case input of
        (c:rst)
            | c == ch   -> Ok ([ch], rst)
            | otherwise -> Err $ ParseError { expected = [ch], found = [c] }
        [] -> Err $ ParseError { expected = [ch], found = "<EOF>" }


instance Exactly String where
    exactly s = \input ->
        case splitAt (length s) input of
            (prefix, rst)
                | prefix == s -> Ok (prefix, rst)
                | otherwise   -> Err $ ParseError { expected = s, found = prefix }


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


sepByStrict :: Parser -> Parser -> Parser
e `sepByStrict` s = e `andThen` ((s `andThen` (sepBy e s)) `orElse` okParser)

sepBy :: Parser -> Parser -> Parser
e `sepBy` s = e `andThen` ((s `andThen` (optional (sepBy e s))) `orElse` okParser)


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

    let result8 = (digit `sepBy` (exactly ',')) "1,2,3,4"
    print result8

    let result9 = (digit `sepBy` (exactly ',')) "1,2,,3,4," -- Parses just fine with any number of commas, including trailing ones
    let result10 = (digit `sepByStrict` (exactly ',')) "1,2,,3,4," -- Does not parse additional commas and the last one
    print result9
    print result10

    let result11 = (digit `sepBy` (exactly ',')) ",1,2,3,4" -- Errors
    print result11


-- vim: et ts=4 sts=4 sw=4
