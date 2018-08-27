module Path.LowLevel.ParserHelpers exposing (Exponent(..), Sign(..), applyExponent, applySign, comma, commaWsp, coordinatePair, delimited, delimitedEndForbidden, digitSequence, exponent, flag, floatingPointConstant, fractionalConstant, ignore, integerConstant, isWhitespace, join, nonNegativeNumber, number, optional, resultToParser, sign, withDefault, wsp)

{-| Helpers for parsing the primitives of SVG path syntax, based on [this W3C grammar](https://www.w3.org/TR/SVG/paths.html#PathDataBNF).
-}

import Char
import Parser exposing (..)
import Parser.Future as Parser exposing (oneOrMore, zeroOrMore)
import Path.LowLevel exposing (Coordinate)



-- Primitives


join : Parser (Parser a) -> Parser a
join =
    Parser.andThen identity


resultToParser : Result String a -> Parser a
resultToParser result =
    case result of
        Err e ->
            Parser.problem e

        Ok v ->
            Parser.succeed v


maybeToParser : Maybe a -> Parser a
maybeToParser result =
    case result of
        Nothing ->
            Parser.problem "a Nothing was converted into a parser"

        Just v ->
            Parser.succeed v


{-| Parse a sequence of values separated by a delimiter

This parser is used to for example parse the comma or whitespace-delimited arguments for a horizontal move

    Parser.run (delimited { delimiter = optional commaWsp, item = number }) "1 2 3 4" == [ 1, 2, 3, 4 ]

-}
delimited : { delimiter : Parser (), item : Parser a } -> Parser (List a)
delimited { delimiter, item } =
    oneOf
        [ item
            |> Parser.andThen (\first -> delimitedEndForbidden item delimiter [ first ])
        , Parser.succeed []
        ]


delimitedEndForbidden : Parser a -> Parser () -> List a -> Parser (List a)
delimitedEndForbidden parseItem delimiter revItems =
    let
        chompRest : a -> Parser (List a)
        chompRest item =
            delimitedEndForbidden parseItem delimiter (item :: revItems)
    in
    oneOf
        [ Parser.backtrackable
            (Parser.succeed (\x -> x)
                |. delimiter
                |. Parser.commit ()
                |= parseItem
                |> andThen chompRest
            )
        , succeed (List.reverse revItems)
        ]


type Sign
    = Plus
    | Minus


sign : Parser Sign
sign =
    oneOf
        [ symbol "-"
            |> ignore (succeed Minus)
        , symbol "+"
            |> ignore (succeed Plus)
        ]


digitSequence : Parser Int
digitSequence =
    Parser.keep oneOrMore Char.isDigit
        |> Parser.andThen (String.toInt >> maybeToParser)



-- |> inContext "digit sequence"


type Exponent
    = Exponent Int


exponent : Parser Exponent
exponent =
    Parser.map Exponent <|
        succeed applySign
            |. oneOf [ symbol "e", symbol "E" ]
            |= withDefault Plus sign
            |= digitSequence


fractionalConstant : Parser Float
fractionalConstant =
    let
        helper left right =
            String.toFloat (String.fromInt left ++ "." ++ String.fromInt right)
                |> maybeToParser
    in
    withDefault 0 digitSequence
        |. symbol "."
        |> Parser.andThen
            (\leftOfDot ->
                Parser.succeed (\rightOfDot -> helper leftOfDot rightOfDot)
                    |= oneOf
                        [ digitSequence
                        , Parser.succeed 0
                        ]
            )
        |> join


applyExponent : Float -> Exponent -> Parser Float
applyExponent float (Exponent exp) =
    String.toFloat (String.fromFloat float ++ "e" ++ String.fromInt exp)
        |> maybeToParser


floatingPointConstant : Parser Float
floatingPointConstant =
    -- inContext "floating point constant" <|
    join <|
        oneOf
            [ backtrackable <|
                succeed applyExponent
                    |= fractionalConstant
                    |= withDefault (Exponent 0) exponent
            , succeed applyExponent
                |= Parser.map toFloat digitSequence
                |= withDefault (Exponent 0) exponent
            ]


integerConstant : Parser Int
integerConstant =
    -- inContext "integer constant" <|
    digitSequence


comma : Parser ()
comma =
    symbol ","


wsp : Parser ()
wsp =
    -- inContext "whitespace" <|
    -- (#x20 | #x9 | #xD | #xA)
    -- previously included \x0D
    oneOf [ symbol " ", symbol "\t", symbol "\n" ]


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\n'


commaWsp : Parser ()
commaWsp =
    -- inContext "comma or whitespace" <|
    oneOf
        [ succeed ()
            |. Parser.ignore oneOrMore isWhitespace
            |. withDefault () comma
            |. Parser.ignore zeroOrMore isWhitespace
        , succeed ()
            |. comma
            |. Parser.ignore zeroOrMore isWhitespace
        ]


flag : Parser Int
flag =
    -- inContext "flag" <|
    oneOf
        [ symbol "1"
            |> Parser.map (\_ -> 1)
        , symbol "0"
            |> Parser.map (\_ -> 0)
        ]


applySign : Sign -> number -> number
applySign currentSign num =
    case currentSign of
        Plus ->
            num

        Minus ->
            -num


number : Parser Float
number =
    oneOf
        [ succeed applySign
            |= withDefault Plus sign
            |= floatingPointConstant
        , succeed applySign
            |= withDefault Plus sign
            |= integerConstant
            |> Parser.map toFloat
        ]


nonNegativeNumber : Parser Float
nonNegativeNumber =
    -- inContext "non-negative number" <|
    oneOf
        [ succeed identity
            |. withDefault () (symbol "+")
            |= floatingPointConstant
        , succeed identity
            |. withDefault () (symbol "+")
            |= integerConstant
            |> Parser.map toFloat
        ]


coordinatePair : Parser Coordinate
coordinatePair =
    -- inContext "coordinate pair" <|
    succeed Tuple.pair
        |= number
        |. optional commaWsp
        |= number



-- Parser Helpers


{-| Try a parser. If it fails, give back the default value
-}
withDefault : a -> Parser a -> Parser a
withDefault default parser =
    oneOf [ parser, succeed default ]


{-| Parse zero or one values of a given parser.
This function is often written as a `?` in grammars, so `int?` is `optional int`
-}
optional : Parser a -> Parser ()
optional parser =
    oneOf
        [ parser
            |> ignore (succeed ())
        , succeed ()
        ]


ignore : Parser keep -> Parser ignore -> Parser keep
ignore keepParser ignoreParser =
    ignoreParser |> Parser.andThen (\_ -> keepParser)



{-
   {-| Ignore everything that came before, start fresh
   -}
   (|-) : Parser ignore -> Parser keep -> Parser keep
   (|-) ignoreParser keepParser =
       map2 (\_ keep -> keep) ignoreParser keepParser
-}
