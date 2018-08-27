module Parser.Future exposing (ignore, inContext, keep, oneOrMore, repeat, zeroOrMore)

import Char exposing (Char)
import Parser exposing ((|=), Parser, Step(..), loop, oneOf)


inContext : context -> Parser a -> Parser a
inContext _ parser =
    parser


repeat : AtLeast -> Parser a -> Parser (List a)
repeat count parser =
    case count of
        ZeroOrMore ->
            repeatHelper parser []

        OneOrMore ->
            oneOf
                [ parser
                    |> Parser.andThen Parser.commit
                    |> Parser.andThen (\firstItem -> repeatHelper parser [ firstItem ])
                , Parser.problem "repeat expected at least one, but got none"
                ]


repeatHelper parser accum =
    oneOf
        [ Parser.backtrackable (parser |> Parser.andThen (\nextItem -> repeatHelper parser (nextItem :: accum)))
        , Parser.succeed (List.reverse accum)
        ]


type AtLeast
    = ZeroOrMore
    | OneOrMore


ignore : AtLeast -> (Char -> Bool) -> Parser ()
ignore amount predicate =
    keep amount predicate
        |> Parser.map (\_ -> ())


keep : AtLeast -> (Char -> Bool) -> Parser String
keep amount predicate =
    case amount of
        ZeroOrMore ->
            oneOf
                [ Parser.backtrackable (Parser.chompWhile predicate)
                    |> Parser.getChompedString
                , Parser.succeed ""
                ]

        OneOrMore ->
            Parser.backtrackable (Parser.chompWhile predicate)
                |> Parser.getChompedString
                |> Parser.andThen
                    (\parsedString ->
                        if String.length parsedString >= 1 then
                            Parser.succeed parsedString

                        else
                            Parser.problem "expected at least one, but got none"
                    )


zeroOrMore =
    ZeroOrMore


oneOrMore =
    OneOrMore


loopList : Parser a -> Parser (List a)
loopList elementParser =
    loop [] (listHelp elementParser)


listHelp : Parser a -> List a -> Parser (Step (List a) (List a))
listHelp elementParser accum =
    oneOf
        [ Parser.succeed (\stmt -> Loop (stmt :: accum))
            |= elementParser
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse accum))
        ]
