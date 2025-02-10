{-# LANGUAGE LambdaCase #-}
module Main where

import Hexpandr
import System.Exit (exitSuccess, exitFailure)

data TestResult = Pass String | Fail String

-- ANSI color codes
colorGreen, colorRed, colorReset :: String
colorGreen = "\x1b[32m"
colorRed = "\x1b[31m"
colorReset = "\x1b[0m"

test :: String -> Bool -> TestResult
test name True = Pass name
test name False = Fail name

testGroup :: String -> [TestResult] -> (String, [TestResult])
testGroup = (,)

runTest :: TestResult -> IO Bool
runTest (Pass n) = do
  putStrLn $ "|   " ++ colorGreen ++ "[\2713] " ++ n ++ colorReset -- U+2713 == check mark
  return True
runTest (Fail n) = do
  putStrLn $ "|   " ++ colorRed ++ "[x] " ++ n ++ colorReset
  return False

runGroup :: (String, [TestResult]) -> IO Int
runGroup (name, ts) = do
  let n :: Int = sum (map testResultToInt ts)
  let color = if n == 0 then colorGreen else colorRed

  -- Group name
  putStrLn $ "| " ++ color ++ name ++ colorReset

  -- Group content
  nFailed <- sum <$> mapM (fmap boolToInt . runTest) ts

  -- Empty line
  putStrLn "| "

  return nFailed
  where
    testResultToInt :: TestResult -> Int
    testResultToInt = \case { Pass _ -> 0; Fail _ -> 1 }

    boolToInt :: Bool -> Int
    boolToInt True = 1
    boolToInt False = 0

runGroups :: [(String, [TestResult])] -> IO Int
runGroups gs = sum <$> mapM runGroup gs

main :: IO ()
main = do
  putStrLn "|-- Running tests"
  putStrLn "|"
  nFailed <- runGroups
    [
      testGroup "Combinators and primitives"
        [ --
          test "Multiple items" $
            parse (many1 alpha `sepBy` char ',') "item,item,item"
              == Ok ["item", "item", "item"] ""
        ],
      testGroup "AST parsing"
        [ --
          test "Parse mapping 0args" $
            parse parseMapping "df mp -> ''"
              == Ok (Mapping (AstIdent "mp") [] "") []
          ,
          test "Parse mapping 1arg" $
            parse parseMapping "df name arg1 -> 'str'"
              == Ok (Mapping (AstIdent "name") [IdentArg (AstIdent "arg1")] "str") []
          ,
          test "Parse mapping 5-args with space" $
            parse parseMapping "df name a1 a2 a3     a4   a5 -> 'str'"
              == Ok (Mapping (AstIdent "name") [
                IdentArg (AstIdent "a1"),
                IdentArg (AstIdent "a2"),
                IdentArg (AstIdent "a3"),
                IdentArg (AstIdent "a4"),
                IdentArg (AstIdent "b5")
                ] "str") []
        ]
    ]
  if nFailed == 0 then do
    putStrLn $ "| " ++ colorGreen ++ "All tests passed" ++ colorReset
    exitSuccess
  else do
    putStrLn $ "| " ++ colorRed ++ show nFailed ++ " tests failed" ++ colorReset
    exitFailure

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
