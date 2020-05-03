module Path.LowLevel.ParserInternal exposing (closepath, command, curveto, curvetoArgument, curvetoArgumentSequence, drawtoCommand, drawtoCommands, ellipticalArc, ellipticalArcArgument, ellipticalArcArgumentSequence, horizontalLineto, horizontalLinetoArgumentSequence, lineto, linetoArgumentSequence, moveToDrawToCommandGroup, moveToDrawToCommandGroups, moveto, movetoArgumentSequence, quadraticBezierCurveto, quadraticBezierCurvetoArgument, quadraticBezierCurvetoArgumentSequence, smoothCurveto, smoothCurvetoArgument, smoothCurvetoArgumentSequence, smoothQuadraticBezierCurveto, smoothQuadraticBezierCurvetoArgumentSequence, svgMixedPath, svgMixedSubPath, verticalLineto, verticalLinetoArgumentSequence)

import Char
import Parser.Advanced as Parser exposing ((|.), (|=), Token(..), chompWhile, oneOf, succeed, symbol)
import Path.LowLevel exposing (Coordinate, DrawTo(..), EllipticalArcArgument, Mode(..), MoveTo(..))
import Path.LowLevel.ParserHelpers exposing (coordinatePair, delimited, flag, isWhitespace, nonNegativeNumber, number, optionalCommaWsp)


type alias Parser a =
    Parser.Parser String String a


endOfInput : Parser ()
endOfInput =
    Parser.end "end of input"


{-| A parser for path data, based on the [specification's grammar](https://www.w3.org/TR/SVG/paths.html#PathDataBNF)
-}
svgMixedPath : Parser (List ( MoveTo, List DrawTo ))
svgMixedPath =
    Parser.succeed identity
        |. chompWhile isWhitespace
        |= moveToDrawToCommandGroups
        |. chompWhile isWhitespace
        |. endOfInput


svgMixedSubPath : Parser ( MoveTo, List DrawTo )
svgMixedSubPath =
    Parser.succeed identity
        |. chompWhile isWhitespace
        |= moveToDrawToCommandGroup
        |. chompWhile isWhitespace
        |. endOfInput


moveToDrawToCommandGroups : Parser (List ( MoveTo, List DrawTo ))
moveToDrawToCommandGroups =
    delimited { item = moveToDrawToCommandGroup, delimiter = chompWhile isWhitespace }


moveToDrawToCommandGroup : Parser ( MoveTo, List DrawTo )
moveToDrawToCommandGroup =
    Parser.inContext "moveto drawto command group" <|
        Parser.succeed
            (\( move, linetos ) drawtos ->
                case linetos of
                    Nothing ->
                        ( move, drawtos )

                    Just lt ->
                        ( move, lt :: drawtos )
            )
            |= moveto
            |. chompWhile isWhitespace
            |= drawtoCommands


drawtoCommands : Parser (List DrawTo)
drawtoCommands =
    Parser.inContext "drawto commands" <|
        delimited { item = drawtoCommand, delimiter = chompWhile isWhitespace }


drawtoCommand : Parser DrawTo
drawtoCommand =
    oneOf drawToCommandOptions


drawToCommandOptions : List (Parser DrawTo)
drawToCommandOptions =
    [ closepath
    , lineto
    , horizontalLineto
    , verticalLineto
    , curveto
    , smoothCurveto
    , quadraticBezierCurveto
    , smoothQuadraticBezierCurveto
    , ellipticalArc
    ]


moveto : Parser ( MoveTo, Maybe DrawTo )
moveto =
    {- moveto has some corner cases

       * if a moveto is followed by extra coordinate pairs, they are interpreted as lineto commands (relative when the moveto is relative, absolute otherwise).
       * the first moveto in a path is always interpreted as absolute (but following linetos are still relative)
    -}
    Parser.inContext "moveto" <|
        command
            { constructor =
                \mode coordinates ->
                    case coordinates of
                        ( c, [] ) ->
                            ( MoveTo mode c, Nothing )

                        ( c, cs ) ->
                            -- cs has at least size 1
                            ( MoveTo mode c, Just (LineTo mode cs) )
            , character = 'm'
            , arguments = movetoArgumentSequence
            }


movetoArgumentSequence : Parser ( Coordinate, List Coordinate )
movetoArgumentSequence =
    delimited { item = coordinatePair, delimiter = optionalCommaWsp }
        |> Parser.andThen
            (\coordinatesList ->
                case coordinatesList of
                    [] ->
                        Parser.problem "moveto argument sequence needs at least one coordinate, got none"

                    x :: xs ->
                        Parser.succeed ( x, xs )
            )


closepath : Parser DrawTo
closepath =
    -- per the w3c spec "Since the Z and z commands take no parameters, they have an identical effect."
    Parser.chompIf
        (\c ->
            case c of
                'z' ->
                    True

                'Z' ->
                    True

                _ ->
                    False
        )
        "close path"
        |> Parser.map (\_ -> ClosePath)
        |> Parser.inContext "closepath"


lineto : Parser DrawTo
lineto =
    Parser.inContext "lineto" <|
        command
            { constructor = LineTo
            , character = 'l'
            , arguments = linetoArgumentSequence
            }


linetoArgumentSequence : Parser (List Coordinate)
linetoArgumentSequence =
    delimited { item = coordinatePair, delimiter = optionalCommaWsp }


horizontalLineto : Parser DrawTo
horizontalLineto =
    Parser.inContext "horizontal lineto" <|
        command
            { constructor = Horizontal
            , character = 'h'
            , arguments = horizontalLinetoArgumentSequence
            }


horizontalLinetoArgumentSequence : Parser (List Float)
horizontalLinetoArgumentSequence =
    delimited { item = number, delimiter = optionalCommaWsp }


verticalLineto : Parser DrawTo
verticalLineto =
    Parser.inContext "vertical lineto" <|
        command
            { constructor = Vertical
            , character = 'v'
            , arguments = verticalLinetoArgumentSequence
            }


verticalLinetoArgumentSequence : Parser (List Float)
verticalLinetoArgumentSequence =
    delimited { item = number, delimiter = optionalCommaWsp }


curveto : Parser DrawTo
curveto =
    Parser.inContext "curveto" <|
        oneOf
            [ succeed (CurveTo Absolute)
                |. symbol (Token "C" "C")
                |. chompWhile isWhitespace
                |= curvetoArgumentSequence
            , succeed (CurveTo Relative)
                |. symbol (Token "c" "c")
                |. chompWhile isWhitespace
                |= curvetoArgumentSequence
            ]


curvetoArgumentSequence : Parser (List ( Coordinate, Coordinate, Coordinate ))
curvetoArgumentSequence =
    delimited { item = curvetoArgument, delimiter = optionalCommaWsp }


curvetoArgument : Parser ( Coordinate, Coordinate, Coordinate )
curvetoArgument =
    succeed (\pair1 pair2 pair3 -> ( pair1, pair2, pair3 ))
        |= coordinatePair
        |. optionalCommaWsp
        |= coordinatePair
        |. optionalCommaWsp
        |= coordinatePair


smoothCurveto : Parser DrawTo
smoothCurveto =
    Parser.inContext "smooth curveto" <|
        command
            { constructor = SmoothCurveTo
            , character = 's'
            , arguments = smoothCurvetoArgumentSequence
            }


smoothCurvetoArgumentSequence : Parser (List ( Coordinate, Coordinate ))
smoothCurvetoArgumentSequence =
    delimited { item = smoothCurvetoArgument, delimiter = optionalCommaWsp }


smoothCurvetoArgument : Parser ( Coordinate, Coordinate )
smoothCurvetoArgument =
    succeed Tuple.pair
        |= coordinatePair
        |. optionalCommaWsp
        |= coordinatePair


quadraticBezierCurveto : Parser DrawTo
quadraticBezierCurveto =
    Parser.inContext "quadratic bezier curveto" <|
        command
            { constructor = QuadraticBezierCurveTo
            , character = 'q'
            , arguments = quadraticBezierCurvetoArgumentSequence
            }


quadraticBezierCurvetoArgumentSequence : Parser (List ( Coordinate, Coordinate ))
quadraticBezierCurvetoArgumentSequence =
    Parser.inContext "quadratic bezier curveto argument sequence" <|
        delimited { item = quadraticBezierCurvetoArgument, delimiter = optionalCommaWsp }


quadraticBezierCurvetoArgument : Parser ( Coordinate, Coordinate )
quadraticBezierCurvetoArgument =
    succeed Tuple.pair
        |= coordinatePair
        |. optionalCommaWsp
        |= coordinatePair


smoothQuadraticBezierCurveto : Parser DrawTo
smoothQuadraticBezierCurveto =
    Parser.inContext "smooth quadratic bezier curveto" <|
        command
            { constructor = SmoothQuadraticBezierCurveTo
            , character = 't'
            , arguments = smoothQuadraticBezierCurvetoArgumentSequence
            }


smoothQuadraticBezierCurvetoArgumentSequence : Parser (List Coordinate)
smoothQuadraticBezierCurvetoArgumentSequence =
    delimited { item = coordinatePair, delimiter = optionalCommaWsp }


ellipticalArc : Parser DrawTo
ellipticalArc =
    Parser.inContext "elliptical arc" <|
        command
            { constructor = EllipticalArc
            , character = 'a'
            , arguments = ellipticalArcArgumentSequence
            }


ellipticalArcArgumentSequence : Parser (List EllipticalArcArgument)
ellipticalArcArgumentSequence =
    delimited { item = ellipticalArcArgument, delimiter = optionalCommaWsp }


ellipticalArcArgument : Parser EllipticalArcArgument
ellipticalArcArgument =
    let
        helper rx ry xAxisRotate arc sweep target =
            case Path.LowLevel.decodeFlags ( arc, sweep ) of
                Just ( arcFlag, direction ) ->
                    Parser.succeed
                        { radii = ( rx, ry )
                        , xAxisRotate = xAxisRotate
                        , arcFlag = arcFlag
                        , direction = direction
                        , target = target
                        }

                Nothing ->
                    Parser.problem "could not parse the arc and sweep flags"
    in
    succeed helper
        |= nonNegativeNumber
        |. optionalCommaWsp
        |= nonNegativeNumber
        |. optionalCommaWsp
        |= number
        |. optionalCommaWsp
        |= flag
        |. optionalCommaWsp
        |= flag
        |. optionalCommaWsp
        |= coordinatePair
        |> Parser.andThen identity


{-| Construct both the absolute and relative parser for a command.
-}
command : { constructor : Mode -> args -> command, character : Char, arguments : Parser args } -> Parser command
command { constructor, character, arguments } =
    oneOf
        [ succeed (constructor Absolute)
            |. symbol (Token (String.fromChar <| Char.toUpper character) (String.fromChar <| Char.toUpper character))
            |. chompWhile isWhitespace
            |= arguments
        , succeed (constructor Relative)
            |. symbol (Token (String.fromChar <| Char.toLower character) (String.fromChar <| Char.toLower character))
            |. chompWhile isWhitespace
            |= arguments
        ]
