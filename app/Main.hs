module Main where

data Result ok err = Ok ok | Err err
    deriving (Eq, Ord, Read, Show)

data ParseError = ParseError { expected :: String, found :: String }
    deriving (Eq, Show)

type ParseResult = Result (String, String) ParseError

type Parser = String -> ParseResult

chr :: Parser
chr = \input -> case input of

  (x:xs) -> (Ok ([x], xs))

  []     -> Err $ ParseError { expected = "any character", found = "<EOI>" }


eof :: Parser
eof = \input -> case input of

  []    -> Ok ([], [])

  (c:_) -> Err $ ParseError { expected = "<EOI>", found = [c] }


andThen :: Parser -> Parser -> Parser
a `andThen` b = \input ->
    case a input of
        Ok (s, rst) -> case b rst of
            Ok (s_b, rst_b) -> Ok (s ++ s_b, rst_b)
        Err err -> Err err


orElse :: Parser -> Parser -> Parser
a `orElse` b = \input ->
    case a input of
        Err err -> b input
        ok -> ok


many :: Parser -> Parser
many p = \input ->
    case p input of
        Ok (s, rst) -> 
            case many p rst of
                Ok (s_b, rst_b) -> Ok (s ++ s_b, rst_b)
        Err err -> Ok ([], input)


main :: IO ()
main = do
    let result1 = eof ""
    print result1  -- Output: Ok []
    
    let result = Main.chr "Hello"
    print result

    let result = eof "Hello"
    print result

    let result = (chr `andThen` chr) "Hello"
    print result

    let result = ((many chr) `andThen` eof) "Hello"
    print result


-- vim: et ts=4 sts=4 sw=4
