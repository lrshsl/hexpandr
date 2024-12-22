module Main where

import Hexpandr

main :: IO ()
main = do
  print $ parse (ident `sepBy1` whitespace) "hey there"
