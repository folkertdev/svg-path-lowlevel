module Path.LowLevel.ParserHelpers exposing (Sign(..), applySign, comma, coordinatePair, delimited, delimitedEndForbidden, flag, fractionalConstant, integerConstant, isWhitespace, nonNegativeNumber, number, optional, optionalCommaWsp, resultToParser, sign, withDefault, wsp)

{-| Helpers for parsing the primitives of SVG path syntax, based on [this W3C grammar](https://www.w3.org/TR/SVG/paths.html#PathDataBNF).
-}

import Char
import Parser.Advanced as Parser exposing ((|.), (|=), Step(..), Token(..), chompIf, chompWhile, oneOf)
import Path.LowLevel exposing (Coordinate)


symbol x =
    Parser.token (Token x "invalid symbol")


type alias Parser a =
    Parser.Parser String String a



-- Primitives


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
            |> Parser.andThen (\first -> Parser.loop [ first ] (delimitedEndForbidden item delimiter))
        , Parser.succeed []
        ]


delimitedEndForbidden : Parser a -> Parser () -> List a -> Parser (Step (List a) (List a))
delimitedEndForbidden parseItem delimiter revItems =
    oneOf
        [ Parser.succeed (\new -> Loop (new :: revItems))
            |. Parser.backtrackable delimiter
            |= parseItem
        , Parser.succeed (Done (List.reverse revItems))
        ]


type Sign
    = Plus
    | Minus


sign : Parser Sign
sign =
    oneOf
        [ symbol "-"
            |> Parser.map (\_ -> Minus)
        , symbol "+"
            |> Parser.map (\_ -> Plus)
        , Parser.succeed Plus
        ]


exponent : Parser ()
exponent =
    Parser.succeed ()
        |. chompIf (\c -> c == 'e' || c == 'E') "exponent"
        |. oneOf [ chompIf (\c -> c == '+' || c == '-') "sign", Parser.succeed () ]
        |. chompIf Char.isDigit "digit"
        |. chompWhile Char.isDigit


parseFloat : String -> Parser Float
parseFloat string =
    case String.toFloat string of
        Nothing ->
            Parser.problem "invalid floating point"

        Just v ->
            Parser.succeed v


fractionalConstant : Parser Float
fractionalConstant =
    Parser.succeed ()
        |. chompIf Char.isDigit "digit"
        |. chompWhile Char.isDigit
        |. symbol "."
        |. chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen parseFloat


floatingPointConstant : Parser Float
floatingPointConstant =
    Parser.succeed ()
        |. chompIf Char.isDigit "digit"
        |. chompWhile Char.isDigit
        |. oneOf
            [ Parser.succeed ()
                |. symbol "."
                |. chompWhile Char.isDigit
            , Parser.succeed ()
            ]
        |. oneOf [ exponent, Parser.succeed () ]
        |> Parser.getChompedString
        |> Parser.andThen parseFloat


integerConstant : Parser Int
integerConstant =
    chompIf Char.isDigit "digit"
        |. chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen (String.toInt >> maybeToParser)


comma : Parser ()
comma =
    symbol ","


wsp : Parser ()
wsp =
    chompIf isWhitespace "whitespace"


isWhitespace : Char -> Bool
isWhitespace char =
    case char of
        ' ' ->
            True

        '\t' ->
            True

        '\n' ->
            True

        _ ->
            False


optionalCommaWsp : Parser ()
optionalCommaWsp =
    Parser.succeed ()
        |. chompWhile isWhitespace
        |. oneOf [ comma, Parser.succeed () ]
        |. chompWhile isWhitespace


flag : Parser Int
flag =
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
    let
        zeroOrMoreDigits =
            chompWhile Char.isDigit

        oneOrMoreDigits =
            chompIf Char.isDigit "digit"
                |. chompWhile Char.isDigit

        parser =
            oneOf
                [ Parser.chompIf (\c -> c == '+' || c == '-') ""
                    |. zeroOrMoreDigits
                    |. oneOf
                        [ symbol "."
                            |. zeroOrMoreDigits
                        , Parser.succeed ()
                        ]
                    |. oneOf [ exponent, Parser.succeed () ]
                , oneOrMoreDigits
                    |. Parser.oneOf
                        [ symbol "."
                            |. zeroOrMoreDigits
                        , Parser.succeed ()
                        ]
                    |. oneOf [ exponent, Parser.succeed () ]
                ]
    in
    parser
        |> Parser.getChompedString
        |> Parser.andThen parseFloat


nonNegativeNumber : Parser Float
nonNegativeNumber =
    Parser.succeed identity
        |. withDefault () (symbol "+")
        |= floatingPointConstant


coordinatePair : Parser Coordinate
coordinatePair =
    -- inContext "coordinate pair" <|
    Parser.succeed Tuple.pair
        |= number
        |. optionalCommaWsp
        |= number



-- Parser Helpers


{-| Try a parser. If it fails, give back the default value
-}
withDefault : a -> Parser a -> Parser a
withDefault default parser =
    oneOf [ parser, Parser.succeed default ]


{-| Parse zero or one values of a given parser.
This function is often written as a `?` in grammars, so `int?` is `optional int`
-}
optional : Parser a -> Parser ()
optional parser =
    oneOf
        [ parser
            |> Parser.map (\_ -> ())
        , Parser.succeed ()
        ]
