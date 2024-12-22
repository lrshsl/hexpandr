module Hexpandr.Parser where

data ParseResult a
  = Ok a String
  | Err String String
  deriving Eq

instance (Show a) => Show (ParseResult a) where
  show pr = case pr of
    Ok parsed rest -> "Ok (Parsed: " ++ show parsed ++ ", left: " ++ show rest ++ ")"
    Err expected found -> "Err (Expected: " ++ show expected ++ ", found: " ++ show found ++ ")"

newtype Parser a = Parser {parse :: String -> ParseResult a}

-- Functor instance for Parser
instance Functor Parser where
  fmap f (Parser p) = Parser $ \input ->
    case p input of
      Ok value rest -> Ok (f value) rest
      Err e found -> Err e found

-- Applicative instance for Parser
instance Applicative Parser where
  pure value = Parser $ \input -> Ok value input
  (Parser pf) <*> (Parser pa) = Parser $ \input ->
    case pf input of
      Ok f rest -> case pa rest of
        Ok a rest' -> Ok (f a) rest'
        Err e found -> Err e found
      Err e found -> Err e found

-- Monad instance for Parser
instance Monad Parser where
  return = pure
  (Parser pa) >>= f = Parser $ \input ->
    case pa input of
      Ok value rest -> parse (f value) rest
      Err e found -> Err e found

