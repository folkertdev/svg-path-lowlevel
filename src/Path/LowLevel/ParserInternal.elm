module Path.LowLevel.ParserInternal exposing (closepath, command, curveto, curvetoArgument, curvetoArgumentSequence, drawtoCommand, drawtoCommands, ellipticalArc, ellipticalArcArgument, ellipticalArcArgumentSequence, horizontalLineto, horizontalLinetoArgumentSequence, lineto, linetoArgumentSequence, moveToDrawToCommandGroup, moveToDrawToCommandGroups, moveto, movetoArgumentSequence, quadraticBezierCurveto, quadraticBezierCurvetoArgument, quadraticBezierCurvetoArgumentSequence, smoothCurveto, smoothCurvetoArgument, smoothCurvetoArgumentSequence, smoothQuadraticBezierCurveto, smoothQuadraticBezierCurvetoArgumentSequence, svgMixedPath, svgMixedSubPath, verticalLineto, verticalLinetoArgumentSequence)

import Char
import Parser.Advanced as Parser exposing ((|.), (|=), Parser, oneOf, succeed, symbol)
import Parser.Future as Parser exposing (oneOrMore, zeroOrMore)
import Path.LowLevel exposing (..)
import Path.LowLevel.ParserHelpers as ParserHelpers exposing (Problem(..), commaWsp, coordinatePair, delimited, flag, isWhitespace, join, nonNegativeNumber, number, optional, withDefault, wsp)


type alias CustomParser a =
    Parser.Parser String ParserHelpers.Problem a


{-| A parser for path data, based on the [specification's grammar](https://www.w3.org/TR/SVG/paths.html#PathDataBNF)
-}
svgMixedPath : CustomParser (List ( MoveTo, List DrawTo ))
svgMixedPath =
    Parser.succeed identity
        |. Parser.chompWhile isWhitespace
        |= withDefault [] moveToDrawToCommandGroups
        |. Parser.chompWhile isWhitespace
        |. Parser.end ExpectedEnd


svgMixedSubPath : CustomParser ( MoveTo, List DrawTo )
svgMixedSubPath =
    Parser.succeed identity
        |. Parser.chompWhile isWhitespace
        |= moveToDrawToCommandGroup
        |. Parser.chompWhile isWhitespace
        |. Parser.end ExpectedEnd


moveToDrawToCommandGroups : CustomParser (List ( MoveTo, List DrawTo ))
moveToDrawToCommandGroups =
    delimited { item = moveToDrawToCommandGroup, delimiter = Parser.chompWhile isWhitespace }


moveToDrawToCommandGroup : CustomParser ( MoveTo, List DrawTo )
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
            |. Parser.chompWhile isWhitespace
            |= withDefault [] drawtoCommands


drawtoCommands : CustomParser (List DrawTo)
drawtoCommands =
    Parser.inContext "drawto commands" <|
        delimited { item = drawtoCommand, delimiter = Parser.chompWhile isWhitespace }


drawtoCommand : CustomParser DrawTo
drawtoCommand =
    oneOf
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


moveto : CustomParser ( MoveTo, Maybe DrawTo )
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


movetoArgumentSequence : CustomParser ( Coordinate, List Coordinate )
movetoArgumentSequence =
    delimited { item = coordinatePair, delimiter = withDefault () commaWsp }
        |> Parser.andThen
            (\coordinatesList ->
                case coordinatesList of
                    [] ->
                        Parser.problem "moveto argument sequence needs at least one coordinate, got none"

                    x :: xs ->
                        Parser.succeed ( x, xs )
            )


closepath : CustomParser DrawTo
closepath =
    -- per the w3c spec "Since the Z and z commands take no parameters, they have an identical effect."
    Parser.inContext "closepath" <|
        oneOf
            [ symbol "z"
                |> Parser.map (\_ -> ClosePath)
            , symbol "Z"
                |> Parser.map (\_ -> ClosePath)
            ]


lineto : CustomParser DrawTo
lineto =
    Parser.inContext "lineto" <|
        command
            { constructor = LineTo
            , character = 'l'
            , arguments = linetoArgumentSequence
            }


linetoArgumentSequence : CustomParser (List Coordinate)
linetoArgumentSequence =
    delimited { item = coordinatePair, delimiter = withDefault () commaWsp }


horizontalLineto : CustomParser DrawTo
horizontalLineto =
    Parser.inContext "horizontal lineto" <|
        command
            { constructor = Horizontal
            , character = 'h'
            , arguments = horizontalLinetoArgumentSequence
            }


horizontalLinetoArgumentSequence : CustomParser (List Float)
horizontalLinetoArgumentSequence =
    delimited { item = number, delimiter = withDefault () commaWsp }


verticalLineto : CustomParser DrawTo
verticalLineto =
    Parser.inContext "vertical lineto" <|
        command
            { constructor = Vertical
            , character = 'v'
            , arguments = verticalLinetoArgumentSequence
            }


verticalLinetoArgumentSequence : CustomParser (List Float)
verticalLinetoArgumentSequence =
    delimited { item = number, delimiter = withDefault () commaWsp }


curveto : CustomParser DrawTo
curveto =
    Parser.inContext "curveto" <|
        command
            { constructor = CurveTo
            , character = 'c'
            , arguments = curvetoArgumentSequence
            }


curvetoArgumentSequence : CustomParser (List ( Coordinate, Coordinate, Coordinate ))
curvetoArgumentSequence =
    delimited { item = curvetoArgument, delimiter = withDefault () commaWsp }


curvetoArgument : CustomParser ( Coordinate, Coordinate, Coordinate )
curvetoArgument =
    succeed (\pair1 pair2 pair3 -> ( pair1, pair2, pair3 ))
        |= coordinatePair
        |. withDefault () commaWsp
        |= coordinatePair
        |. withDefault () commaWsp
        |= coordinatePair


smoothCurveto : CustomParser DrawTo
smoothCurveto =
    Parser.inContext "smooth curveto" <|
        command
            { constructor = SmoothCurveTo
            , character = 's'
            , arguments = smoothCurvetoArgumentSequence
            }


smoothCurvetoArgumentSequence : CustomParser (List ( Coordinate, Coordinate ))
smoothCurvetoArgumentSequence =
    delimited { item = smoothCurvetoArgument, delimiter = withDefault () commaWsp }


smoothCurvetoArgument : CustomParser ( Coordinate, Coordinate )
smoothCurvetoArgument =
    succeed Tuple.pair
        |= coordinatePair
        |. withDefault () commaWsp
        |= coordinatePair


quadraticBezierCurveto : CustomParser DrawTo
quadraticBezierCurveto =
    Parser.inContext "quadratic bezier curveto" <|
        command
            { constructor = QuadraticBezierCurveTo
            , character = 'q'
            , arguments = quadraticBezierCurvetoArgumentSequence
            }


quadraticBezierCurvetoArgumentSequence : CustomParser (List ( Coordinate, Coordinate ))
quadraticBezierCurvetoArgumentSequence =
    Parser.inContext "quadratic bezier curveto argument sequence" <|
        delimited { item = quadraticBezierCurvetoArgument, delimiter = withDefault () commaWsp }



--Parser.repeat oneOrMore (quadraticBezierCurvetoArgument |. withDefault () commaWsp)


quadraticBezierCurvetoArgument : CustomParser ( Coordinate, Coordinate )
quadraticBezierCurvetoArgument =
    succeed Tuple.pair
        |= coordinatePair
        |. withDefault () commaWsp
        |= coordinatePair


smoothQuadraticBezierCurveto : CustomParser DrawTo
smoothQuadraticBezierCurveto =
    Parser.inContext "smooth quadratic bezier curveto" <|
        command
            { constructor = SmoothQuadraticBezierCurveTo
            , character = 't'
            , arguments = smoothQuadraticBezierCurvetoArgumentSequence
            }


smoothQuadraticBezierCurvetoArgumentSequence : CustomParser (List Coordinate)
smoothQuadraticBezierCurvetoArgumentSequence =
    delimited { item = coordinatePair, delimiter = withDefault () commaWsp }


ellipticalArc : CustomParser DrawTo
ellipticalArc =
    Parser.inContext "elliptical arc" <|
        command
            { constructor = EllipticalArc
            , character = 'a'
            , arguments = ellipticalArcArgumentSequence
            }


ellipticalArcArgumentSequence : CustomParser (List EllipticalArcArgument)
ellipticalArcArgumentSequence =
    delimited { item = ellipticalArcArgument, delimiter = withDefault () commaWsp }


ellipticalArcArgument : CustomParser EllipticalArcArgument
ellipticalArcArgument =
    let
        helper rx ry xAxisRotate arc sweep target =
            case decodeFlags ( arc, sweep ) of
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
        |. withDefault () commaWsp
        |= nonNegativeNumber
        |. withDefault () commaWsp
        |= number
        |. commaWsp
        |= flag
        |. withDefault () commaWsp
        |= flag
        |. withDefault () commaWsp
        |= coordinatePair
        |> join


{-| Construct both the absolute and relative parser for a command.
-}
command : { constructor : Mode -> args -> command, character : Char, arguments : CustomParser args } -> CustomParser command
command { constructor, character, arguments } =
    oneOf
        [ succeed (constructor Absolute)
            |. symbol (String.fromChar <| Char.toUpper character)
            |. Parser.ignore zeroOrMore isWhitespace
            |= arguments
        , succeed (constructor Relative)
            |. symbol (String.fromChar <| Char.toLower character)
            |. Parser.ignore zeroOrMore isWhitespace
            |= arguments
        ]
