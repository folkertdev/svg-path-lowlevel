module Parser.Future exposing (ignore, keep, oneOrMore, repeat, zeroOrMore)

import Char exposing (Char)
import Parser.Advanced as Parser exposing ((|=), Parser, Step(..), loop, oneOf)


type CountProblem
    = CountProblem { expected : AtLeast, got : Int }


repeat : AtLeast -> Parser context CountProblem a -> Parser context CountProblem (List a)
repeat count parser =
    case count of
        ZeroOrMore ->
            repeatHelper parser []

        OneOrMore ->
            oneOf
                [ parser
                    |> Parser.andThen Parser.commit
                    |> Parser.andThen (\firstItem -> repeatHelper parser [ firstItem ])
                , Parser.problem (CountProblem { expected = OneOrMore, got = 0 })
                ]


repeatHelper parser accum =
    oneOf
        [ Parser.backtrackable (parser |> Parser.andThen (\nextItem -> repeatHelper parser (nextItem :: accum)))
        , Parser.succeed (List.reverse accum)
        ]


type AtLeast
    = ZeroOrMore
    | OneOrMore


ignore : AtLeast -> (Char -> Bool) -> Parser c CountProblem ()
ignore amount predicate =
    keep amount predicate
        |> Parser.map (\_ -> ())


keep : AtLeast -> (Char -> Bool) -> Parser c CountProblem String
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
                            Parser.problem (CountProblem { expected = OneOrMore, got = 0 })
                    )


zeroOrMore =
    ZeroOrMore


oneOrMore =
    OneOrMore


loopList : Parser c p a -> Parser c p (List a)
loopList elementParser =
    loop [] (listHelp elementParser)


listHelp : Parser c p a -> List a -> Parser c p (Step (List a) (List a))
listHelp elementParser accum =
    oneOf
        [ Parser.succeed (\stmt -> Loop (stmt :: accum))
            |= elementParser
        , Parser.succeed ()
            |> Parser.map (\_ -> Done (List.reverse accum))
        ]
