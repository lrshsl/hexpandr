module Main where

data Result ok err = Ok ok | Err err
    deriving (Eq, Ord, Read, Show)

data ParseError = ParseError { expected :: String, found :: String }
    deriving (Eq, Show)

type ParseResult = Result String ParseError

newtype Parser a = Parser {
  runParser :: String -> (String, ParseResult)
}


any :: Parser Char
any = Parser $ \input -> case input of

  (x:xs) -> (xs, Ok [x])

  []     -> ("", Err $ ParseError { expected = "any character", found = "eof" })


eof :: Parser ()
eof = Parser $ \input -> case input of

  []    -> ("", Ok [])

  (c:_) -> (input, Err $ ParseError { expected = "eof", found = [c] })


main :: IO ()
main = do
    let result1 = runParser eof ""
    print result1  -- Output: Ok []
    
    let result2 = runParser Main.any "Hello"
    print result2


-- vim: et ts=4 sts=4 sw=4
