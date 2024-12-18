
module Main (Main.main) where

import Hexpandr

main :: IO ()
main = do

    ---------------------------------------
    --- Single characters
    ---------------------------------------

    putStrLn "\n-- Single characters --"

    let parser1 = eof ""
    print $ parser1 == Ok ("", "")

    let parser2 = anyChar
    print $ parser2 "Hello" == Ok ("H", "ello")

    let parser3 = eof
    print $ parser3 "Hello" == Err ("<EOI>", "H" )

    let parser4 = anyChar `andThen` anyChar
    print $ parser4 "Hello" == Ok ("He", "llo")

    let parser5 = many anyChar `andThen` eof
    print $ parser5 "Hello" == Ok ("Hello", "")

    let parser6 = chr '4'
    print $ parser6 "Hello" == Err ("4", "H")

    let parser7 = exactly "Hel"
    print $ parser7 "Hello" == Ok ("Hel", "lo")


    ---------------------------------------
    --- sepBy and sepByStrict
    ---------------------------------------

    putStrLn "\n-- SepBy parsers --"

    let sepByParser = digit `sepBy` chr ','
    let sepByStrictParser = digit `sepByStrict` chr ','

    print $ sepByParser "1,2,3,4" == Ok ("1,2,3,4", "")
    print $ sepByStrictParser "1,2,3,4" == Ok ("1,2,3,4", "")

    -- sepBy parses just fine with any number of commas, including trailing ones
    print $ sepByParser "1,2,,3,4," == Ok ("1,2,,3,4,", "")

    -- Does not parse additional commas and the last one
    print $ sepByStrictParser "1,2,,3,4," == Ok ("1,2", ",,3,4,")


    ---------------------------------------
    --- predefined alphanumerical+ sets
    ---------------------------------------

    putStrLn "\n-- Alphanumerical --"

    let parser11 = digit `sepBy` chr ','
    print $ parser11 ",1,2,3,4" == Ok (",1,2,3,4", "")

    let identch = oneOf [digit, alpha, chr '_']
    let parser12 = identch `sepBy` chr ','
    print $ parser12 "a,2,Z,4" == Ok ("a,2,Z,4", "")

    let parser13 = identch `sepBy` chr ','
    print $ parser13 "a,2a,Z,4" == Ok ("a,2", "a,Z,4")

    let parser14 = many identch `sepBy` chr ','
    print $ parser14 "a,2a,Z,nice_3" == Ok ("a,2a,Z,nice_3", "")

    let parser15 = defaultIdent `sepBy` chr ','
    print $ parser15 "a,Z,nice_3,a2" == Ok ("a,Z,nice_3,a2", "")


    ---------------------------------------
    --- Further examples
    ---------------------------------------

    putStrLn "\n-- Further examples --"

    let identP = alpha `andThen` many (oneOf [alpha, digit, chr '_'])
    print $ identP "some_ident42" == Ok ("some_ident42", "")

    let floatP = many1 digit `andThen` optional (chr '.' `andThen` many digit)
    print $ floatP "32.01" == Ok ("32.01", "")
    print $ floatP "1" == Ok ("1", "")
    print $ floatP "a" == Err (['0' .. '9'], "a")

    let intP = many1 digit

    let arrayElement = oneOf [identP, floatP, intP]
    let arrayP = chr '[' `andThen` (arrayElement `sepBy` chr ' ') `andThen` chr ']'

    print $ arrayP "[a 3.5 b 90]" == Ok ("[a 3.5 b 90]", "")

    putStrLn "\nTests Done\n"

