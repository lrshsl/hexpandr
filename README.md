# H-Expandr

## Library

Parser combinators and types that are needed for the executable.

```haskell
main = do
    let identP = alpha `andThen` many (oneOf [alpha, digit, chr '_'])
    let floatP = many1 digit `andThen` optional (chr '.' `andThen` many digit)
    let intP = many1 digit

    let arrayElement = oneOf [identP, floatP, intP]
    let arrayP = chr '[' `andThen` (arrayElement `sepBy` chr ' ') `andThen` chr ']'

    print $ arrayP "[a 3.5 b 90]"   -- Ok ("[a 3.5 b 90]", "")
```

For more examples take a look at the [test directory](test/).

## Executable

WIP

