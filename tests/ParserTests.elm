module ParserTests exposing (commaWhitespaceParsing, commands, coordinatePairParsingProblems, fuzzCoordinate, fuzzMode, fuzzMoveTo, parseTest, primitives, suite, whitespaceParsing)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, bool, int)
import Parser exposing ((|.), (|=))
import Parser.Future as Parser
import Path.LowLevel exposing (..)
import Path.LowLevel.ParserHelpers exposing (..)
import Path.LowLevel.ParserInternal exposing (..)
import Test exposing (..)


suite : Test
suite =
    describe "svg path syntax parser in elm" <|
        let
            serious =
                "M600,350 l10,10 l20,20 Z"

            curves =
                """M 213.1,6.7 C 110.6,4.9,67.5-9.5,36.9,6.7
          c -32.4-14.4-73.7,0-88.1,30.6
          C 110.6,4.9,67.5-9.5,36.9,6.7
          C 2.8,22.9-13.4,62.4,13.5,110.9
          C 33.3,145.1,67.5,170.3,125,217
          c 59.3-46.7,93.5-71.9,111.5-106.1
          C 263.4,64.2,247.2,22.9,213.1,6.7
          z
          """
        in
        [ test "moveto drawto command group" <|
            \_ ->
                Parser.run moveToDrawToCommandGroup serious
                    |> Expect.equal
                        (Ok
                            ( MoveTo Absolute ( 600, 350 )
                            , [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], ClosePath ]
                            )
                        )
        , test "moveto drawto command groups lines" <|
            \_ ->
                Parser.run moveToDrawToCommandGroups serious
                    |> Expect.equal
                        (Ok
                            [ ( MoveTo Absolute ( 600, 350 )
                              , [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], ClosePath ]
                              )
                            ]
                        )
        , test "moveto drawto command groups curves" <|
            \_ ->
                Parser.run moveToDrawToCommandGroups curves
                    |> Expect.equal
                        (Ok
                            [ ( MoveTo Absolute ( 213.1, 6.7 )
                              , [ CurveTo Absolute [ ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) ) ]
                                , CurveTo Relative [ ( ( -32.4, -14.4 ), ( -73.7, 0 ), ( -88.1, 30.6 ) ) ]
                                , CurveTo Absolute [ ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) ) ]
                                , CurveTo Absolute [ ( ( 2.8, 22.9 ), ( -13.4, 62.4 ), ( 13.5, 110.9 ) ) ]
                                , CurveTo Absolute [ ( ( 33.3, 145.1 ), ( 67.5, 170.3 ), ( 125, 217 ) ) ]
                                , CurveTo Relative [ ( ( 59.3, -46.7 ), ( 93.5, -71.9 ), ( 111.5, -106.1 ) ) ]
                                , CurveTo Absolute [ ( ( 263.4, 64.2 ), ( 247.2, 22.9 ), ( 213.1, 6.7 ) ) ]
                                , ClosePath
                                ]
                              )
                            ]
                        )
        , test "relative moveto 0,0" <|
            \_ ->
                Parser.run moveto "m 0,0"
                    |> Expect.equal (Ok ( MoveTo Relative ( 0, 0 ), Nothing ))
        , test "lineto argument sequence" <|
            \_ ->
                Parser.run linetoArgumentSequence "10,10 20,20"
                    |> Expect.equal
                        (Ok [ ( 10, 10 ), ( 20, 20 ) ])
        , test "lineto command with multiple arguments " <|
            \_ ->
                Parser.run lineto "l 10,10 20,20"
                    |> Expect.equal
                        (Ok (LineTo Relative [ ( 10, 10 ), ( 20, 20 ) ]))
        ]


whitespaceParsing : Test
whitespaceParsing =
    describe "svg path parsing uses whitespace permissively" <|
        List.map
            (\( label, serious ) ->
                test label <|
                    \_ ->
                        Parser.run svgMixedPath serious
                            |> Expect.equal
                                (Ok
                                    [ ( MoveTo Absolute ( 600, 350 )
                                      , [ LineTo Relative [ ( 10, 10 ) ], LineTo Relative [ ( 20, 20 ) ], ClosePath ]
                                      )
                                    ]
                                )
            )
            [ ( "no spacing", "M600,350l10,10l20,20Z" )
            , ( "some spaces", "M600,350 l10,10 l20,20 Z" )
            , ( "well spaced", "M 600, 350 l 10, 10 l 20, 20 Z" )
            , ( "crazy spaced", "M   600  ,  350 l  10  , 10 l 20  , 20  Z" )
            ]


coordinatePairParsingProblems : Test
coordinatePairParsingProblems =
    describe "coordinate pair parsing problems"
        [ test "quadraticBezierCurvetoArgumentSequence" <|
            \_ ->
                Parser.run quadraticBezierCurvetoArgumentSequence "1,1,1,1"
                    |> Expect.equal (Ok [ ( ( 1, 1 ), ( 1, 1 ) ) ])
        , test "coordinate pair" <|
            \_ ->
                Parser.run coordinatePair "1,1,1,1"
                    |> Expect.equal (Ok ( 1, 1 ))
        , test "coordinate pair and trailing commaWsp" <|
            \_ ->
                Parser.run (coordinatePair |. commaWsp) "1,1,1,1"
                    |> Expect.equal (Ok ( 1, 1 ))
        , test "coordinate pairs and separating commaWsp" <|
            \_ ->
                Parser.run (Parser.succeed Tuple.pair |= coordinatePair |. commaWsp |= coordinatePair) "1,1,1,1"
                    |> Expect.equal (Ok ( ( 1, 1 ), ( 1, 1 ) ))
        , test "coordinate pairs and separating commaWsp using repeat" <|
            \_ ->
                Parser.run (Parser.repeat Parser.oneOrMore (coordinatePair |. withDefault () commaWsp)) "1,1,1,1"
                    |> Expect.equal (Ok [ ( 1, 1 ), ( 1, 1 ) ])
        , test "quadraticBezierCurvetoArgument" <|
            \_ ->
                Parser.run quadraticBezierCurvetoArgument "1,1,1,1"
                    |> Expect.equal (Ok ( ( 1, 1 ), ( 1, 1 ) ))
        , test "long example with extra commas and nonnegative numbers" <|
            \_ ->
                Parser.run svgMixedPath "M0,-41.421356 A41.421356,41.421356 0 1 1 41.421356,0 L24.142136,0 A24.142136,24.142136 0 0 1 0,-24.142136 Z"
                    |> Expect.equal
                        (Ok
                            [ ( MoveTo Absolute ( 0, -41.421356 )
                              , [ EllipticalArc Absolute
                                    [ { radii = ( 41.421356, 41.421356 )
                                      , xAxisRotate = 0
                                      , arcFlag = LargestArc
                                      , direction = CounterClockwise
                                      , target = ( 41.421356, 0 )
                                      }
                                    ]
                                , LineTo Absolute [ ( 24.142136, 0 ) ]
                                , EllipticalArc Absolute
                                    [ { radii = ( 24.142136, 24.142136 )
                                      , xAxisRotate = 0
                                      , arcFlag = SmallestArc
                                      , direction = CounterClockwise
                                      , target = ( 0, -24.142136 )
                                      }
                                    ]
                                , ClosePath
                                ]
                              )
                            ]
                        )
        , test "coordinate pair without comma with minus" <|
            \_ ->
                "67.5-9.5"
                    |> Parser.run coordinatePair
                    |> Expect.equal (Ok ( 67.5, -9.5 ))
        , test "cubic example" <|
            \_ ->
                "C 110.6,4.9,67.5-9.5,36.9,6.7"
                    |> Parser.run curveto
                    |> Expect.equal (Ok <| CurveTo Absolute [ ( ( 110.6, 4.9 ), ( 67.5, -9.5 ), ( 36.9, 6.7 ) ) ])
        ]


commaWhitespaceParsing : Test
commaWhitespaceParsing =
    describe "allow extra commas between coordinate pairs" <|
        List.map
            (\( label, serious ) ->
                test label <|
                    \_ ->
                        Parser.run svgMixedPath serious
                            |> Expect.equal
                                (Ok
                                    [ ( MoveTo Absolute ( 150, 50 )
                                      , [ QuadraticBezierCurveTo Absolute [ ( ( 100, 50 ), ( 200, 100 ) ) ] ]
                                      )
                                    ]
                                )
            )
            [ ( "with extra comma", "M150,50Q100,50,200,100" )
            , ( "without extra comma", "M150,50Q100,50 200,100" )
            ]


fuzzMoveTo : Fuzzer MoveTo
fuzzMoveTo =
    Fuzz.map2 MoveTo fuzzMode fuzzCoordinate


fuzzMode : Fuzzer Mode
fuzzMode =
    Fuzz.map
        (\value ->
            if value then
                Absolute

            else
                Relative
        )
        bool


fuzzCoordinate : Fuzzer ( Float, Float )
fuzzCoordinate =
    Fuzz.map2 Tuple.pair
        (Fuzz.map toFloat int)
        (Fuzz.map toFloat int)


{-| Test a parser against its expected result
-}
parseTest : Parser.Parser a -> String -> a -> Test
parseTest parser string output =
    test ("parsing `" ++ string ++ "`") <|
        \_ ->
            Parser.run parser string
                |> Expect.equal (Ok output)


primitives : Test
primitives =
    describe "primitive value parsers"
        [ parseTest nonNegativeNumber "25" 25
        , parseTest digitSequence "42" 42
        , parseTest digitSequence "42X" 42
        , parseTest digitSequence "42 " 42
        , parseTest floatingPointConstant "42.0" 42
        , parseTest floatingPointConstant "42.0X" 42
        , parseTest floatingPointConstant "42.0 " 42
        , parseTest floatingPointConstant "42e0" 42
        , parseTest floatingPointConstant "42e1" 420
        , parseTest floatingPointConstant "42e1X" 420
        , parseTest (delimited { item = floatingPointConstant, delimiter = commaWsp }) "" []
        , parseTest (delimited { item = floatingPointConstant, delimiter = commaWsp }) "45" [ 45 ]
        , parseTest (delimited { item = floatingPointConstant, delimiter = commaWsp }) "45 45" [ 45, 45 ]
        ]


commands : Test
commands =
    let
        ellipticalArcExample =
            { radii = ( 25, 25 )
            , xAxisRotate = -30
            , arcFlag = SmallestArc
            , direction = CounterClockwise
            , target = ( 50, -25 )
            }
    in
    describe "parsing individual commands"
        [ parseTest moveto "M0,0" ( MoveTo Absolute ( 0, 0 ), Nothing )
        , parseTest moveto "M0.3,0" ( MoveTo Absolute ( 0.3, 0 ), Nothing )
        , parseTest moveto "m0,0" ( MoveTo Relative ( 0, 0 ), Nothing )
        , parseTest moveto "M0,0 20,20" ( MoveTo Absolute ( 0, 0 ), Just (LineTo Absolute [ ( 20, 20 ) ]) )
        , parseTest moveto "m0,0 20,20" ( MoveTo Relative ( 0, 0 ), Just (LineTo Relative [ ( 20, 20 ) ]) )
        , parseTest lineto "L0,0" (LineTo Absolute [ ( 0, 0 ) ])
        , parseTest lineto "l0,0" (LineTo Relative [ ( 0, 0 ) ])
        , parseTest horizontalLineto "H0 1 2 3" (Horizontal Absolute [ 0, 1, 2, 3 ])
        , parseTest horizontalLineto "h0 1 2 3" (Horizontal Relative [ 0, 1, 2, 3 ])
        , parseTest verticalLineto "V0 1 2 3" (Vertical Absolute [ 0, 1, 2, 3 ])
        , parseTest verticalLineto "v0 1 2 3" (Vertical Relative [ 0, 1, 2, 3 ])
        , parseTest closepath "Z" ClosePath
        , parseTest closepath "z" ClosePath
        , parseTest curveto "C100,100 250,100 250,200" (CurveTo Absolute [ ( ( 100, 100 ), ( 250, 100 ), ( 250, 200 ) ) ])
        , parseTest curveto "c100,100 250,100 250,200" (CurveTo Relative [ ( ( 100, 100 ), ( 250, 100 ), ( 250, 200 ) ) ])
        , parseTest smoothCurveto "S400,300 400,200" (SmoothCurveTo Absolute [ ( ( 400, 300 ), ( 400, 200 ) ) ])
        , parseTest smoothCurveto "s400,300 400,200" (SmoothCurveTo Relative [ ( ( 400, 300 ), ( 400, 200 ) ) ])
        , parseTest quadraticBezierCurveto "Q400,50 600,300" (QuadraticBezierCurveTo Absolute [ ( ( 400, 50 ), ( 600, 300 ) ) ])
        , parseTest quadraticBezierCurveto "q400,50 600,300" (QuadraticBezierCurveTo Relative [ ( ( 400, 50 ), ( 600, 300 ) ) ])
        , parseTest smoothQuadraticBezierCurveto "T1000,300" (SmoothQuadraticBezierCurveTo Absolute [ ( 1000, 300 ) ])
        , parseTest smoothQuadraticBezierCurveto "t1000,300" (SmoothQuadraticBezierCurveTo Relative [ ( 1000, 300 ) ])
        , parseTest ellipticalArc "A25,25 -30 0,1 50,-25" (EllipticalArc Absolute [ ellipticalArcExample ])
        , parseTest ellipticalArc "a25,25 -30 0,1 50,-25" (EllipticalArc Relative [ ellipticalArcExample ])
        ]
