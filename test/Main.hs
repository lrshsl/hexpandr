
module Main (Main.main) where

import Hexpandr

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

    let result6 = chr '4' "Hello"
    print result6

    let result7 = exactly "Hel" "Hello"
    print result7

    let result8 = (digit `sepBy` chr ',') "1,2,3,4"
    print result8

    let result9 = (digit `sepBy` chr ',') "1,2,,3,4," -- Parses just fine with any number of commas, including trailing ones
    let result10 = (digit `sepByStrict` chr ',') "1,2,,3,4," -- Does not parse additional commas and the last one
    print result9
    print result10

    let result11 = (digit `sepBy` chr ',') ",1,2,3,4" -- Errors
    print result11

    let identch = oneOf [digit, alpha, chr '_']
    let result12 = (identch `sepBy` chr ',') "a,2,Z,4"
    print result12

    let result13 = (identch `sepBy` chr ',') "a,2a,Z,4"
    print result13

    let result14 = (many identch `sepBy` chr ',') "a,2a,Z,nice_3"
    print result14

    let result15 = (defaultIdent `sepBy` chr ',') "a,Z,nice_3,a2"
    print result15

