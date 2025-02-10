module Main where

import Hexpandr

main :: IO ()
main = do
  print $ parse parseMapping "df name arg -> 'str'"
  print $ parse (ident `sepBy1` whitespace) "hey there"
