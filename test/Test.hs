module Main where

import Hexpandr

import System.Exit (exitFailure, exitSuccess)

data TestResult = Pass | Fail String

-- ANSI color codes
colorGreen, colorRed, colorReset :: String
colorGreen = "\x1b[32m"
colorRed = "\x1b[31m"
colorReset = "\x1b[0m"

-- Test function with colored output
simpleTest :: String -> Bool -> IO TestResult
simpleTest name condition =
  if condition
    then do
      putStrLn $ colorGreen ++ "Test passed: " ++ name ++ colorReset
      return Pass
    else do
      putStrLn $ colorRed ++ "Test failed: " ++ name ++ colorReset
      return (Fail name)

runTests :: [IO TestResult] -> IO ()
runTests tests = do
  results <- sequence tests
  let failed = [name | Fail name <- results]
  if null failed
    then do
      putStrLn $ colorGreen ++ "All tests passed!" ++ colorReset
      sequence_ [return ()]
      exitSuccess
    else do
      putStrLn $ colorRed ++ "Some tests failed: " ++ show failed ++ colorReset
      putStrLn $ colorRed ++ show (length failed) ++ " tests failed." ++ colorReset
      exitFailure

main :: IO ()
main = do
  print $ parse parseMapping "df name arg -> 'str'"
  -- runTests [ simpleTest "Multiple items" $

  --     parse (many1 alpha `sepBy` char ',') "item,item,item"

  --   == Ok ["item", "item", "item"] ""]

    -- , simpleTest "Parse mapping" $
    --   parse parseMapping "df name arg1 -> 'str'"
    --   == Ok (Mapping (AstIdent "name")) []

  -- ---------------------------------------
  -- --- Single characters
  -- ---------------------------------------

  -- putStrLn "\n-- Single characters --"

  -- test (parse eoi "" == Ok ("", "")) []

  -- let parser2 = anyChar
  -- print $ parse parser2 "Hello" -- == Ok ("H", "ello")

  -- let parser3 = eoi
  -- print $ parse parser3 "Hello" -- == Err ("<EOI>", "H")

  -- let parser4 = anyChar `andThen` anyChar
  -- print $ parse parser4 "Hello" -- == Ok ("He", "llo")

  -- let parser5 = many anyChar `followedBy` eoi
  -- print $ parse parser5 "Hello" -- == Ok ("Hello", "")

  -- let parser6 = char '4'
  -- print $ parse parser6 "Hello" -- == Err ("4", "H")

  -- let parser7 = exactly "Hel"
  -- print $ parse parser7 "Hello" -- == Ok ("Hel", "lo")

  -- ---------------------------------------
  -- --- sepBy and sepByStrict
  -- ---------------------------------------

  -- putStrLn "\n-- SepBy parsers --"

  -- let sepByParser = digit `sepBy` char ','

  -- -- parses just fine with any number of commas, including trailing ones
  -- print $ parse sepByParser "1,2,3,4" -- == Ok ("1,2,3,4", "")
  -- print $ parse sepByParser "1,2,,3,4," -- == Ok ("1,2,,3,4,", "")
  -- print $ parse sepByParser ",1," -- == Ok (",1,2,,3,4,", "")

  -- ---------------------------------------
  -- --- predefined alphanumerical+ sets
  -- ---------------------------------------

  -- putStrLn "\n-- Alphanumerical --"

  -- let parser11 = digit `sepBy` char ','
  -- print $ parse parser11 ",1,2,3,4" -- == Ok (",1,2,3,4", "")

  -- let identch = choice [digit, alpha, char '_']
  -- let parser12 = identch `sepBy` char ','
  -- print $ parse parser12 "a,2,Z,4" -- == Ok ("a,2,Z,4", "")

  -- let parser13 = identch `sepBy` char ','
  -- print $ parse parser13 "a,2a,Z,4" -- == Ok ("a,2", "a,Z,4")

  -- let parser14 = many identch `sepBy` char ','
  -- print $ parse parser14 "a,2a,Z,nice_3" -- == Ok ("a,2a,Z,nice_3", "")

  -- let parser15 = ident `sepBy` char ','
  -- print $ parse parser15 "a,Z,nice_3,a2" -- == Ok ("a,Z,nice_3,a2", "")

  -- ---------------------------------------
  -- --- Further examples
  -- ---------------------------------------

  -- putStrLn "\n-- Further examples --"

  -- let identP = (:) <$> alpha <*> many (choice [alpha, digit, char '_'])
  -- print $ parse identP "some_ident42" -- == Ok ("some_ident42", "")

  -- let floatP = fmap flatten $ (:) <$> many1 digit <*> optional ((:) <$> char '.' <*> many digit)
  -- print $ parse floatP "32.01" -- == Ok ("32.01", "")
  -- print $ parse floatP "1" -- == Ok ("1", "")
  -- print $ parse floatP "a" -- == Err (['0' .. '9'], "a")

  -- let intP = many1 digit

  -- let arrayElement = choice [identP, floatP, intP]
  -- let arrayInner = arrayElement `sepBy` whitespace
  -- let arrayP = silent (char '[') >> arrayInner  >> silent (char ']')

  -- print $ parse arrayP "[a 3.5 b 90]" -- == Ok ("[a 3.5 b 90]", "")

  -- putStrLn "\nTests Done\n"
