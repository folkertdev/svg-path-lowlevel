module Path.LowLevel.ParserHelpers exposing (Exponent(..), Problem(..), Sign(..), applyExponent, applySign, comma, commaWsp, coordinatePair, delimited, delimitedEndForbidden, digitSequence, exponent, flag, floatingPointConstant, fractionalConstant, ignore, integerConstant, isWhitespace, join, nonNegativeNumber, number, optional, resultToParser, sign, withDefault, wsp)

{-| Helpers for parsing the primitives of SVG path syntax, based on [this W3C grammar](https://www.w3.org/TR/SVG/paths.html#PathDataBNF).
-}

import Char
import Parser.Advanced as Parser exposing (..)
import Parser.Future as Parser exposing (oneOrMore, zeroOrMore)
import Path.LowLevel exposing (Coordinate)


type Problem
    = ExpectingMinus
    | ExpectingPlus
    | ExpectingExponent
    | ExpectingDot
    | ExpectingComma
    | ExpectingWhitespace
    | ExpectingFlag
    | ExpectedIntegerSequence { got : String }
    | ExpectedFloat { got : String }
    | ExpectedEnd



-- Primitives


join : Parser c p (Parser c p a) -> Parser c p a
join =
    Parser.andThen identity


resultToParser : Result p a -> Parser c p a
resultToParser result =
    case result of
        Err e ->
            Parser.problem e

        Ok v ->
            Parser.succeed v


maybeToParser : Maybe a -> Parser c String a
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
delimited : { delimiter : Parser c p (), item : Parser c p a } -> Parser c p (List a)
delimited { delimiter, item } =
    oneOf
        [ item
            |> Parser.andThen (\first -> delimitedEndForbidden item delimiter [ first ])
        , Parser.succeed []
        ]


delimitedEndForbidden : Parser c p a -> Parser c p () -> List a -> Parser c p (List a)
delimitedEndForbidden parseItem delimiter revItems =
    let
        chompRest : a -> Parser c p (List a)
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


plus =
    Parser.Token "-" ExpectingPlus


minus =
    Parser.Token "+" ExpectingMinus


sign : Parser c Problem Sign
sign =
    oneOf
        [ symbol minus
            |> ignore (succeed Minus)
        , symbol plus
            |> ignore (succeed Plus)
        ]


digitSequence : Parser c Problem Int
digitSequence =
    let
        validator rawString =
            case String.toInt rawString of
                Just value ->
                    Parser.succeed value

                Nothing ->
                    Parser.problem (ExpectedIntegerSequence { got = rawString })
    in
    Parser.chompWhile Char.isDigit
        |> Parser.getChompedString
        |> Parser.andThen validator


type Exponent
    = Exponent Int


exponent : Parser c Problem Exponent
exponent =
    Parser.map Exponent <|
        succeed applySign
            |. oneOf [ symbol (Token "e" ExpectingExponent), symbol (Token "E" ExpectingExponent) ]
            |= withDefault Plus sign
            |= digitSequence


fractionalConstant : Parser c Problem Float
fractionalConstant =
    let
        helper left right =
            let
                rawString =
                    String.fromInt left ++ "." ++ String.fromInt right
            in
            case String.toFloat rawString of
                Nothing ->
                    Parser.problem (ExpectedFloat { got = rawString })

                Just value ->
                    Parser.succeed value
    in
    Parser.succeed helper
        |= withDefault 0 digitSequence
        |. symbol (Token "." ExpectingDot)
        |= withDefault 0 digitSequence
        |> join


applyExponent : Float -> Exponent -> Parser c Problem Float
applyExponent float (Exponent exp) =
    let
        rawString =
            String.fromFloat float ++ "e" ++ String.fromInt exp
    in
    case String.toFloat rawString of
        Nothing ->
            Parser.problem (ExpectedFloat { got = rawString })

        Just value ->
            Parser.succeed value


floatingPointConstant : Parser c Problem Float
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


integerConstant : Parser c Problem Int
integerConstant =
    -- inContext "integer constant" <|
    digitSequence


comma : Parser c Problem ()
comma =
    symbol (Token "," ExpectingComma)


wsp : Parser c Problem ()
wsp =
    -- inContext "whitespace" <|
    -- (#x20 | #x9 | #xD | #xA)
    -- previously included \x0D
    oneOf
        [ symbol (Token " " ExpectingWhitespace)
        , symbol (Token "\t" ExpectingWhitespace)
        , symbol (Token "\n" ExpectingWhitespace)
        ]


isWhitespace : Char -> Bool
isWhitespace char =
    char == ' ' || char == '\t' || char == '\n'


commaWsp : Parser c Problem ()
commaWsp =
    -- inContext "comma or whitespace" <|
    oneOf
        [ succeed ()
            |. Parser.chompWhile isWhitespace
            |. withDefault () comma
            |. Parser.chompWhile isWhitespace
        , succeed ()
            |. comma
            |. Parser.chompWhile isWhitespace
        ]


flag : Parser c Problem Int
flag =
    -- inContext "flag" <|
    oneOf
        [ symbol (Token "1" ExpectingFlag)
            |> Parser.map (\_ -> 1)
        , symbol (Token "0" ExpectingFlag)
            |> Parser.map (\_ -> 0)
        ]


applySign : Sign -> number -> number
applySign currentSign num =
    case currentSign of
        Plus ->
            num

        Minus ->
            -num


number : Parser c Problem Float
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


nonNegativeNumber : Parser c Problem Float
nonNegativeNumber =
    -- inContext "non-negative number" <|
    oneOf
        [ succeed identity
            |. withDefault () (symbol (Token "+" ExpectingPlus))
            |= floatingPointConstant
        , succeed identity
            |. withDefault () (symbol (Token "+" ExpectingPlus))
            |= integerConstant
            |> Parser.map toFloat
        ]


coordinatePair : Parser c Problem Coordinate
coordinatePair =
    -- inContext "coordinate pair" <|
    succeed Tuple.pair
        |= number
        |. optional commaWsp
        |= number



-- Parser Helpers


{-| Try a parser. If it fails, give back the default value
-}
withDefault : a -> Parser c p a -> Parser c p a
withDefault default parser =
    oneOf [ parser, succeed default ]


{-| Parse zero or one values of a given parser.
This function is often written as a `?` in grammars, so `int?` is `optional int`
-}
optional : Parser c p a -> Parser c p ()
optional parser =
    oneOf
        [ parser
            |> ignore (succeed ())
        , succeed ()
        ]


ignore : Parser c p keep -> Parser c p ignore -> Parser c p keep
ignore keepParser ignoreParser =
    ignoreParser |> Parser.andThen (\_ -> keepParser)



{-
   {-| Ignore everything that came before, start fresh
   -}
   (|-) : Parser ignore -> Parser keep -> Parser keep
   (|-) ignoreParser keepParser =
       map2 (\_ keep -> keep) ignoreParser keepParser
-}
